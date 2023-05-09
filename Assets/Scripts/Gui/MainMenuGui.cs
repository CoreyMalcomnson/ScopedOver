using System.Collections;
using System.Collections.Generic;
using Unity.Netcode;
using UnityEngine;

public class MainMenuGui : MonoBehaviour
{
    [SerializeField] private GameObject mainMenu;

    private void Hide()
    {
        mainMenu.SetActive(false);
    }

    public void StartHost()
    {
        NetworkManager.Singleton.StartHost();
        Hide();
    }

    public void StartClient()
    {
        NetworkManager.Singleton.StartClient();
        Hide();
    }

    public void ExitGame()
    {
        Application.Quit();
    }
        
}
