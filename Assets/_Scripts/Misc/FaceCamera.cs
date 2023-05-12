using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class FaceCamera : MonoBehaviour
{
    [SerializeField] private bool invert;
    [SerializeField] private bool flat;

    private void Update()
    {
        Vector3 directionToCamera = (Camera.main.transform.position - transform.position).normalized;

        if (flat)
            directionToCamera.y = 0;

        transform.forward = invert ? -directionToCamera : directionToCamera;
    }
}
