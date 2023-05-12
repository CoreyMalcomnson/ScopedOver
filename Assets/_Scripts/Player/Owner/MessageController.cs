using UnityEngine;
using System.Collections;
using Unity.Netcode;
using System.Collections.Generic;
using TMPro;
using UnityEngine.UI;
using System.Threading.Tasks;

public class MessageController : NetworkBehaviour
{
    public static bool Focused => instance.chatInput.isFocused;

    private static MessageController instance;

    [SerializeField] private GameObject chatParent;

    [SerializeField] private ScrollRect scrollRect;
    [SerializeField] private Transform scrollContent;
    [SerializeField] private TMP_Text chatMessagePrefab;
    [SerializeField] private TMP_InputField chatInput;

    [SerializeField] private int maxMessageHistory = 10;

    private Queue<TMP_Text> messageHistory;

    private void Awake()
    {
        chatParent.SetActive(false);
    }

    public override void OnNetworkSpawn()
    {
        if (!IsOwner)
        {
            this.enabled = false;
            return;
        }

        chatParent.SetActive(true);
        instance = this;
        messageHistory = new();
        chatInput.onValueChanged.AddListener(OnChatInputChanged);
        chatInput.onDeselect.AddListener(delegate { Clear(); });
    }

    private void Update()
    {
        if (InputManager.Local.OpenChat)
        {
            Select();
        }
    }

    private void Select()
    {
        chatInput.interactable = true;
        chatInput.ActivateInputField();
    }

    private async void Clear()
    {
        // Do this because there's a weird bug with clicking and setting InputField to not interactable on the same frame
        // "Attempting to select while already selecting an object."
        await Task.Yield(); 

        chatInput.text = "";
        chatInput.interactable = false;
    }

    private void OnChatInputChanged(string newValue)
    {
        if (!newValue.EndsWith("\n")) return;

        string newMessage = newValue.Remove(newValue.Length - 1);
        MessageManager.Instance.SendMessageServerRPC(newMessage);

        Clear();
}

    public static void AddNewMessage(string message)
    {
        if (instance.messageHistory.Count >= instance.maxMessageHistory)
        {
            TMP_Text oldMessage = instance.messageHistory.Dequeue();
            Destroy(oldMessage.gameObject);
        }

        TMP_Text newMessage = Instantiate(instance.chatMessagePrefab, instance.scrollContent);
        newMessage.text = message;
        instance.messageHistory.Enqueue(newMessage);
    }
}
