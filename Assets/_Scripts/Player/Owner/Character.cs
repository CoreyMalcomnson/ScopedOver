using System;
using System.Collections;
using System.Collections.Generic;
using Unity.Netcode;
using UnityEngine;
using UnityEngine.Events;

public class Character : NetworkBehaviour
{
    public static Character Local { get; private set; }

    public Vector3 MovementDirection { get; private set; }
    public bool IsMoving => !Health.IsDead && (InputManager.Local.Vertical != 0 || InputManager.Local.Horizontal != 0);

    [field: SerializeField] public Transform BodyTransform { get; private set; }
    [field: SerializeField] public Health Health { get; private set; }

    [SerializeField] private CharacterController controller;

    [SerializeField] private float movementSpeed = 5f;
    [SerializeField] private float rotationSpeed = 10f;

    [SerializeField] private LayerMask groundLayerMask;

    private Transform cameraTransform;
    private Vector3 directionToMouse;

    public override void OnNetworkSpawn()
    {
        NetworkObjectManager.Instance.AddCharacter(OwnerClientId, this);

        if (IsOwner)
        {
            Local = this;
        }
    }

    public override void OnNetworkDespawn()
    {
        NetworkObjectManager.Instance.RemoveCharacter(OwnerClientId);
    }

    private void Start()
    {
        cameraTransform = Camera.main.transform;
    }

    private void Update()
    {
        if (!IsOwner) return;
        if (Health.IsDead) return;

        HandleRotation();
        HandleMovement();
    }

    private void HandleMovement()
    {
        if (!IsMoving) return;

        // Get forward based on Camera
        Vector3 forwardDirection = cameraTransform.forward;
        forwardDirection.y = 0;
        forwardDirection.Normalize();

        Vector3 rightDirection = cameraTransform.right;
        rightDirection.y = 0;
        rightDirection.Normalize();

        // Movement Direction
        MovementDirection = (forwardDirection * InputManager.Local.Vertical +
            rightDirection * InputManager.Local.Horizontal).normalized;

        Vector3 movement = (MovementDirection * movementSpeed) + (Vector3.down * 9f);

        // Move
        controller.Move(movement * Time.deltaTime);
    }

    private void HandleRotation()
    {
        if (Physics.Raycast(Camera.main.ScreenPointToRay(InputManager.Local.MousePosition), out RaycastHit hitInfo, float.MaxValue, groundLayerMask))
        {
            directionToMouse = (hitInfo.point - BodyTransform.position).normalized;
            directionToMouse.y = 0;
        }

        // Lerp Rotation
        BodyTransform.forward = Vector3.Lerp(BodyTransform.forward, directionToMouse, Time.deltaTime * rotationSpeed);
    }
}
