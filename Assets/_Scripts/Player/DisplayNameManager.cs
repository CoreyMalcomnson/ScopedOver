using UnityEngine;
using System.Collections;
using Unity.Netcode;
using TMPro;
using Unity.Collections;
using System;

public class DisplayNameManager : NetworkBehaviour
{
    
    [SerializeField] TMP_Text displayNameText;

    public override void OnNetworkSpawn()
    {
        if (IsOwner)
        {
            displayNameText.enabled = false;
        }

        Player player = NetworkObjectManager.Instance.GetPlayer(OwnerClientId);
        NetworkVariable<FixedString64Bytes> usernameNetworkVar = player.GetUsernameNetworkVar();
        usernameNetworkVar.OnValueChanged += PlayerUsernameChanged;

        UpdateDisplayName(usernameNetworkVar.Value.ToString());
    }

    public override void OnNetworkDespawn()
    {
        Player player = NetworkObjectManager.Instance.GetPlayer(OwnerClientId);
        if (player != null)
        {
            NetworkVariable<FixedString64Bytes> username = player.GetUsernameNetworkVar();
            username.OnValueChanged -= PlayerUsernameChanged;
        }
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
