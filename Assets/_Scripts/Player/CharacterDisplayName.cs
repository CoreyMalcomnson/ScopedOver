using UnityEngine;
using System.Collections;
using Unity.Netcode;
using TMPro;
using Unity.Collections;
using System;

public class CharacterDisplayName : NetworkBehaviour
{
    
    [SerializeField] TMP_Text displayNameText;

    public override void OnNetworkSpawn()
    {
        if (IsOwner)
        {
            displayNameText.enabled = false;
        }

        Player player = NetworkObjectManager.Instance.GetPlayer(OwnerClientId);
        NetworkVariable<FixedString64Bytes> username = player.GetUsername();
        username.OnValueChanged += PlayerUsernameChanged;

        UpdateDisplayName(username.Value.ToString());
    }

    public override void OnNetworkDespawn()
    {
        Player player = NetworkObjectManager.Instance.GetPlayer(OwnerClientId);
        NetworkVariable<FixedString64Bytes> username = player.GetUsername();
        username.OnValueChanged -= PlayerUsernameChanged;
    }

    private void PlayerUsernameChanged(FixedString64Bytes previousValue, FixedString64Bytes newValue)
    {
        UpdateDisplayName(newValue.ToString());
    }

    private void UpdateDisplayName(string newUsername)
    {
        displayNameText.text = newUsername;
    }
}
