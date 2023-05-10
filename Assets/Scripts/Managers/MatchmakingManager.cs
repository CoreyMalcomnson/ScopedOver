using System;
using System.Threading.Tasks;
using TMPro;
using Unity.Netcode;
using Unity.Netcode.Transports.UTP;
using Unity.Services.Authentication;
using Unity.Services.Core;
using Unity.Services.Relay;
using Unity.Services.Relay.Models;
using UnityEngine;

public class MatchmakingManager : MonoBehaviour
{
    const string MISSING_USERNAME_MESSAGE = "You must enter a username before creating or joining a game!";
    const string MISSING_CODE_MESSAGE = "You must enter a code before trying to join a game!";
    const string SPACE_IN_USERNAME_MESSAGE = "You can't have spaces in your username!";

    [SerializeField] private TMP_Text messageText;
    [SerializeField] private TMP_Text codeText;
    [SerializeField] private TMP_InputField codeInput;
    [SerializeField] private TMP_InputField usernameInput;
    [SerializeField] private GameObject buttons;

    private static MatchmakingManager instance;

    private UnityTransport transport;
    private const int MaxPlayers = 30;

    private async void Awake()
    {
        instance = this;

        // Begin authentication
        transport = FindObjectOfType<UnityTransport>();
        
        buttons.SetActive(false);

        await Authenticate();

        buttons.SetActive(true);
    }

    private async Task Authenticate()
    {
        codeText.text = "Initializing Unity Services...";
        await UnityServices.InitializeAsync();

        codeText.text = "Authenticating Anonymously...";
        await AuthenticationService.Instance.SignInAnonymouslyAsync();

        codeText.text = "Online!";
    }

    public async void CreateGame()
    {
        if (String.IsNullOrEmpty(usernameInput.text))
        {
            messageText.text = MISSING_USERNAME_MESSAGE;
            return;
        }

        if (usernameInput.text.Contains(" "))
        {
            messageText.text = SPACE_IN_USERNAME_MESSAGE;
            return;
        }

        buttons.SetActive(false);

        Allocation a = await RelayService.Instance.CreateAllocationAsync(MaxPlayers);
        codeText.text = await RelayService.Instance.GetJoinCodeAsync(a.AllocationId);

        transport.SetHostRelayData(a.RelayServer.IpV4, (ushort)a.RelayServer.Port, a.AllocationIdBytes, a.Key, a.ConnectionData);

        NetworkManager.Singleton.StartHost();
    }

    public async void JoinGame()
    {
        if (String.IsNullOrEmpty(usernameInput.text))
        {
            messageText.text = MISSING_USERNAME_MESSAGE;
            return;
        }

        if (usernameInput.text.Contains(" "))
        {
            messageText.text = SPACE_IN_USERNAME_MESSAGE;
            return;
        }

        if (String.IsNullOrEmpty(codeInput.text))
        {
            messageText.text = MISSING_CODE_MESSAGE;
            return;
        }

        buttons.SetActive(false);

        JoinAllocation a = await RelayService.Instance.JoinAllocationAsync(codeInput.text);

        transport.SetClientRelayData(a.RelayServer.IpV4, (ushort)a.RelayServer.Port, a.AllocationIdBytes, a.Key, a.ConnectionData, a.HostConnectionData);

        NetworkManager.Singleton.StartClient();
    }

    public static string GetUsernameInput()
    {
        return instance.usernameInput.text;
    }
}
