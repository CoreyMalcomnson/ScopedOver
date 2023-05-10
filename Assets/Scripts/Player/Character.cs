using UnityEngine;
using Unity.Netcode;
using TMPro;

public class Character : NetworkBehaviour
{
    public static Character Local;

    [SerializeField] TMP_Text displayNameText;

    public override void OnNetworkSpawn()
    {
        NetworkObjectManager.Instance.AddCharacter(OwnerClientId, this);

        if (IsOwner)
        {
            Local = this;
            displayNameText.enabled = false;
        }

        UpdateDisplayName();
    }

    public override void OnNetworkDespawn()
    {
        NetworkObjectManager.Instance.RemoveCharacter(OwnerClientId);
    }

    public void UpdateDisplayName()
    {
        Player player = NetworkObjectManager.Instance.GetPlayer(OwnerClientId);

        if (player != null)
            displayNameText.text = player.GetUsername();
    }

    public void TeleportToOtherCharacter(Character otherCharacter)
    {
        transform.position = otherCharacter.transform.position;
    }
}
