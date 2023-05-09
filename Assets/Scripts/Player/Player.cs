using System;
using System.Collections;
using System.Collections.Generic;
using Unity.Collections;
using Unity.Netcode;
using UnityEngine;

public class Player : NetworkBehaviour
{
    public static Player Local;

    [SerializeField] private Character characterPrefab;

    private NetworkVariable<FixedString64Bytes> username
        = new NetworkVariable<FixedString64Bytes>(writePerm: NetworkVariableWritePermission.Owner);

    public override void OnNetworkSpawn()
    {
        NetworkObjectManager.Instance.AddPlayer(OwnerClientId, this);

        if (IsOwner)
        {
            Local = this;
            username.Value = SimpleMatchmaking.Username;
        }
    }

    public override void OnNetworkDespawn()
    {
        NetworkObjectManager.Instance.RemovePlayer(OwnerClientId);
    }

    [ServerRpc(RequireOwnership = true)]
    public void SpawnCharacterServerRPC()
    {
        Character character = NetworkObjectManager.Instance.GetCharacter(OwnerClientId);

        if (character != null)
            character.GetComponent<NetworkObject>().Despawn();

        character = Instantiate(characterPrefab, Vector3.zero, Quaternion.identity);
        character.GetComponent<NetworkObject>().SpawnWithOwnership(OwnerClientId);
    }

    public string GetUsername()
    {
        return username.Value.ToString();
    }
}
