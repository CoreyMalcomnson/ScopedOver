using UnityEngine;
using Unity.Netcode;
using TMPro;

public class Character : NetworkBehaviour
{
    public static Character Local;

    [SerializeField] TMP_Text usernameText;

    public override void OnNetworkSpawn()
    {
        NetworkObjectManager.Instance.AddCharacter(OwnerClientId, this);

        if (IsOwner)
        {
            Local = this;
        }

        UpdateUsername();
    }

    public override void OnNetworkDespawn()
    {
        NetworkObjectManager.Instance.RemoveCharacter(OwnerClientId);
    }

    public void UpdateUsername()
    {
        Player player = NetworkObjectManager.Instance.GetPlayer(OwnerClientId);

        if (player != null)
            usernameText.text = player.GetUsername();
    }
}
