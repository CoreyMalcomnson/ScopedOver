using System;
using System.Collections;
using System.Collections.Generic;
using Unity.Netcode;
using UnityEngine;

public class PlayerMovementController : NetworkBehaviour
{
    public static PlayerMovementController Local { get; private set; }

    public Vector3 MovementDirection { get; private set; }
    public bool IsMoving { get; private set; }

    [field: SerializeField] public Transform BodyTransform { get; private set; }

    [SerializeField] private CharacterController controller;
    [SerializeField] private Transform cameraTransform;

    [SerializeField] private float movementSpeed = 5f;
    [SerializeField] private float rotationSpeed = 10f;

    [SerializeField] private LayerMask groundLayerMask;

    private Vector3 directionToMouse;

    private void Update()
    {
        if (!IsOwner) return;

        Local = this;

        IsMoving = PlayerInputManager.Local.Vertical != 0 || PlayerInputManager.Local.Horizontal != 0;

        HandleRotation();
        HandleMovement();
    }

    private void HandleMovement()
    {
        if (!IsOwner) return;
        if (!IsMoving) return;

        // Get forward based on Camera
        Vector3 forwardDirection = cameraTransform.forward;
        forwardDirection.y = 0;
        forwardDirection.Normalize();

        Vector3 rightDirection = cameraTransform.right;
        rightDirection.y = 0;
        rightDirection.Normalize();

        // Movement Direction
        MovementDirection = (forwardDirection * PlayerInputManager.Local.Vertical +
            rightDirection * PlayerInputManager.Local.Horizontal).normalized;

        // Move
        controller.Move(MovementDirection * movementSpeed * Time.deltaTime);
    }

    private void HandleRotation()
    {
        if (!IsOwner) return;
        if (Physics.Raycast(Camera.main.ScreenPointToRay(PlayerInputManager.Local.MousePosition), out RaycastHit hitInfo, float.MaxValue, groundLayerMask))
        {
            directionToMouse = (hitInfo.point - BodyTransform.position).normalized;
            directionToMouse.y = 0;
        }

        // Lerp Rotation
        BodyTransform.forward = Vector3.Lerp(BodyTransform.forward, directionToMouse, Time.deltaTime * rotationSpeed);
    }
}
