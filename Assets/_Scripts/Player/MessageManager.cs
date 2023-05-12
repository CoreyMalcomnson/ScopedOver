using UnityEngine;
using System.Collections;
using Unity.Netcode;
using System.Collections.Generic;
using TMPro;
using UnityEngine.UI;

public class MessageManager : NetworkBehaviour
{
    public static MessageManager Instance;

    [SerializeField] private NetworkObject bearPrefab;

    public override void OnNetworkSpawn()
    {
        Instance = this;
    }

    [ClientRpc]
    public void ReplicateMessageClientRPC(string message)
    {
        MessageController.AddNewMessage(message);
    }

    [ServerRpc(RequireOwnership = false)]
    public void SendMessageServerRPC(string message, ServerRpcParams serverRpcParams = default)
    {
        if (string.IsNullOrEmpty(message)) return;


        ulong clientId = serverRpcParams.Receive.SenderClientId;
        Player player = NetworkObjectManager.Instance.GetPlayer(clientId);
        if (player == null) return;

        if (CheckForCommand(clientId, message))
        {
            return;
        }

        ReplicateMessageClientRPC($"{player.GetUsernameNetworkVar().Value}: {message}");
    }

    private bool CheckForCommand(ulong clientId, string message)
    {
        if (!message.StartsWith("/"))
            return false;

        if (message.Contains("/respawn"))
        {
            Player player = NetworkObjectManager.Instance.GetPlayer(clientId);
            if (player != null)
            {
                player.SpawnCharacter();
            }

            return true;
        }

        if (message.Contains("/tp"))
        {
            string[] args = message.Split(" ");
            if (args.Length >= 2)
            {
                Character character = NetworkObjectManager.Instance.GetCharacter(clientId);
                if (character != null)
                {
                    Character otherCharacter = NetworkObjectManager.Instance.GetCharacter(args[1]);
                    if (otherCharacter != null)
                    {
                        character.transform.position = otherCharacter.transform.position;
                        return true;
                    }
                }
            }
        }

        if (message.Contains("/setname"))
        {
            string[] args = message.Split(" ");
            if (args.Length >= 2)
            {
                Player player = NetworkObjectManager.Instance.GetPlayer(clientId);
                if (player != null)
                {
                    player.SetUsername(args[1]);
                    return true;
                }
            }
        }

        if (message.Contains("/bear"))
        {
            string[] args = message.Split(" ");
            if (args.Length >= 2)
            {
                Character character = NetworkObjectManager.Instance.GetCharacter(clientId);
                if (character != null)
                {
                    if (int.TryParse(args[1], out int numberOfBears))
                    {
                        if (numberOfBears < 0 || numberOfBears > 10000)
                            return false;

                        for (int i = 0; i < numberOfBears; i++)
                        {
                            NetworkObject bear = Instantiate(
                            bearPrefab,
                            character.transform.position + new Vector3(
                                Random.Range(-5, 5),
                                character.transform.position.y,
                                Random.Range(-5, 5)),
                            Quaternion.identity);

                            bear.Spawn();
                        }
                        
                    }
                }
            }
        }

        return false;
    }

}
