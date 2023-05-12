using System.Collections;
using System.Collections.Generic;
using Unity.Netcode;
using UnityEngine;

public class PlayerInputManager : NetworkBehaviour
{
    public static PlayerInputManager Local { get; private set; }

    public Vector3 MousePosition { get; private set; }

    public bool Chat { get; private set; }

    public bool Fire { get; private set; }
    public bool AltFire { get; private set; }

    public float Horizontal { get; private set; }
    public float Vertical { get; private set; }
    public float MouseX { get; private set; }
    public float MouseY { get; private set; }
    public float Scroll { get; private set; }

    public override void OnNetworkSpawn()
    {
        if (!IsOwner)
        {
            this.enabled = false;
            return;
        }

        Local = this;
    }

    private void Update()
    {
        if (MessageController.Focused)
        {
            ClearInput();
        } else
        {
            UpdateInput();
        }
    }

    private void ClearInput()
    {
        Chat = default;
        MousePosition = default;
        Fire = default;
        AltFire = default;
        Horizontal = default;
        Vertical = default;
        MouseX = default;
        MouseY = default;
        Scroll = default;
    }

    private void UpdateInput()
    {
        Chat = Input.GetKeyDown(KeyCode.T);
        MousePosition = Input.mousePosition;
        Fire = Input.GetMouseButton(0);
        AltFire = Input.GetMouseButton(1);
        Horizontal = Input.GetAxis("Horizontal");
        Vertical = Input.GetAxis("Vertical");
        MouseX = Input.GetAxis("Mouse X");
        MouseY = Input.GetAxis("Mouse Y");
        Scroll = Input.GetAxis("Mouse ScrollWheel");
    }
}
