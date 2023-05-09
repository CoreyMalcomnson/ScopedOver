using System;
using System.Collections;
using System.Collections.Generic;
using Unity.Netcode;
using UnityEngine;

public class PlayerCameraController : NetworkBehaviour
{
    public static PlayerCameraController Local { get; private set; }

    [SerializeField] private Transform targetTransform;

    [SerializeField] private LayerMask zoomCollisionLayerMask;
    [SerializeField] private float zoomCollisionBuffer = 0.95f;

    [SerializeField] private float mouseSensitivity = 1;
    [SerializeField] private float zoomSensitivity = 1;

    [SerializeField] private float pitchMin = -90;
    [SerializeField] private float pitchMax = 90;

    [SerializeField] private float zoomMin = 5;
    [SerializeField] private float zoomMax = 20;

    private Transform cameraTransform;

    private float pitch;
    private float yaw;
    private float zoom;

    public override void OnNetworkSpawn()
    {
        if (!IsOwner)
        {
            this.enabled = false;
            return;
        }

        Local = this;
    }

    private void Start()
    {
        cameraTransform = Camera.main.transform;
        pitch = Mathf.Lerp(pitchMin, pitchMax, 0.5f);
        zoom = Mathf.Lerp(zoomMin, zoomMax, 0.5f); ;
    }

    private void LateUpdate()
    {
        // Rotation
        if (PlayerInputManager.Local.AltFire)
        {
            pitch = Mathf.Clamp(
                            pitch - PlayerInputManager.Local.MouseY * mouseSensitivity,
                            pitchMin, pitchMax);

            yaw = yaw + PlayerInputManager.Local.MouseX * mouseSensitivity;

        }

        // Zoom
        zoom = Mathf.Clamp(
                    zoom + PlayerInputManager.Local.Scroll * zoomSensitivity,
                    zoomMin, zoomMax);

        // Check for collision
        bool hit = Physics.Raycast(
            targetTransform.position,
            (cameraTransform.position - targetTransform.position).normalized,
            out RaycastHit hitInfo,
            zoom,
            zoomCollisionLayerMask
        );

        float appliedZoom = (hit ? hitInfo.distance * zoomCollisionBuffer : zoom);

        // Apply Position and Rotation
        Vector3 cameraDirection = Quaternion.Euler(pitch, yaw, 0) * Vector3.forward;
        cameraTransform.position = targetTransform.position - cameraDirection * appliedZoom;
        cameraTransform.eulerAngles = new Vector3(pitch, yaw, 0);
    }
}
