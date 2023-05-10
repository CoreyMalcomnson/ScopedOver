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
        = new NetworkVariable<FixedString64Bytes>(writePerm: NetworkVariableWritePermission.Server);

    public override void OnNetworkSpawn()
    {
        NetworkObjectManager.Instance.AddPlayer(OwnerClientId, this);

        if (IsOwner)
        {
            Local = this;
            SpawnCharacterServerRPC();
            InitializeUsernameServerRPC(MatchmakingManager.GetUsernameInput());
        }

        username.OnValueChanged += OnUsernameChanged;
    }

    public override void OnNetworkDespawn()
    {
        NetworkObjectManager.Instance.RemovePlayer(OwnerClientId);
    }

    [ServerRpc(RequireOwnership = true)]
    public void SpawnCharacterServerRPC()
    {
        SpawnCharacter();
    }

    [ServerRpc(RequireOwnership = true)]
    private void InitializeUsernameServerRPC(FixedString64Bytes newUsername)
    {
        username.Value = newUsername;
    }

    public void SpawnCharacter()
    {
        Character character = NetworkObjectManager.Instance.GetCharacter(OwnerClientId);

        if (character != null)
            character.GetComponent<NetworkObject>().Despawn();

        character = Instantiate(characterPrefab, Vector3.zero, Quaternion.identity);
        character.GetComponent<NetworkObject>().SpawnWithOwnership(OwnerClientId);
    }

    private void OnUsernameChanged(FixedString64Bytes previousValue, FixedString64Bytes newValue)
    {
        Character character = NetworkObjectManager.Instance.GetCharacter(OwnerClientId);

        if (character != null)
            character.UpdateDisplayName();
    }

    public string GetUsername()
    {
        return username.Value.ToString();
    }

    public void SetUsername(string newUsername)
    {
        username.Value = newUsername;
    }
}
