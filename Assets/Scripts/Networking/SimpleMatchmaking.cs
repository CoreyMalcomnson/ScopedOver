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

public class SimpleMatchmaking : MonoBehaviour
{
    public static string Username;

    [SerializeField] private TMP_Text codeText;
    [SerializeField] private TMP_InputField codeInput;
    [SerializeField] private TMP_InputField usernameInput;
    [SerializeField] private GameObject buttons;

    private UnityTransport transport;
    private const int MaxPlayers = 30;

    private async void Awake()
    {
        // Track username
        usernameInput.onValueChanged.AddListener((string newValue) =>
        {
            Username = usernameInput.text;
        });

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
        buttons.SetActive(false);

        Allocation a = await RelayService.Instance.CreateAllocationAsync(MaxPlayers);
        codeText.text = await RelayService.Instance.GetJoinCodeAsync(a.AllocationId);

        transport.SetHostRelayData(a.RelayServer.IpV4, (ushort)a.RelayServer.Port, a.AllocationIdBytes, a.Key, a.ConnectionData);

        NetworkManager.Singleton.StartHost();
    }

    public async void JoinGame()
    {
        buttons.SetActive(false);

        JoinAllocation a = await RelayService.Instance.JoinAllocationAsync(codeInput.text);

        transport.SetClientRelayData(a.RelayServer.IpV4, (ushort)a.RelayServer.Port, a.AllocationIdBytes, a.Key, a.ConnectionData, a.HostConnectionData);

        NetworkManager.Singleton.StartClient();
    }
}
