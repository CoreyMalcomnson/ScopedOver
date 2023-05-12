using System.Collections;
using System.Collections.Generic;
using Unity.Netcode;
using UnityEngine;

public class InputManager : NetworkBehaviour
{
    public static InputManager Local { get; private set; }

    public Vector3 MousePosition { get; private set; }

    public bool OpenChat { get; private set; }
    public bool ToggleWeapon { get; private set; }

    public bool Fire { get; private set; }
    public bool AltFire { get; private set; }

    public bool FireDown { get; private set; }
    public bool AltFireDown { get; private set; }

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
        OpenChat = default;
        ToggleWeapon = default;
        MousePosition = default;
        Fire = default;
        AltFire = default;
        FireDown = default;
        AltFireDown = default;
        Horizontal = default;
        Vertical = default;
        MouseX = default;
        MouseY = default;
        Scroll = default;
    }

    private void UpdateInput()
    {
        OpenChat = Input.GetKeyDown(KeyCode.T);
        ToggleWeapon = Input.GetKeyDown(KeyCode.Alpha1);
        MousePosition = Input.mousePosition;
        Fire = Input.GetMouseButton(0);
        AltFire = Input.GetMouseButton(1);
        FireDown = Input.GetMouseButtonDown(0);
        AltFireDown = Input.GetMouseButtonDown(1);
        Horizontal = Input.GetAxis("Horizontal");
        Vertical = Input.GetAxis("Vertical");
        MouseX = Input.GetAxis("Mouse X");
        MouseY = Input.GetAxis("Mouse Y");
        Scroll = Input.GetAxis("Mouse ScrollWheel");
    }
}
