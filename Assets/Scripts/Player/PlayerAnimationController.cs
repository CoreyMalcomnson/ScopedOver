using System.Collections;
using System.Collections.Generic;
using Unity.Netcode;
using UnityEngine;

public class PlayerAnimationController : NetworkBehaviour
{
    [SerializeField] private Animator animator;

    [SerializeField] private float animationDampTime = 0.1f;

    private void Update()
    {
        if (!IsOwner) return;

        Vector3 movementDirection = PlayerMovementController.Local.MovementDirection;
        Transform bodyTransform = PlayerMovementController.Local.BodyTransform;
        bool isMoving = PlayerMovementController.Local.IsMoving;

        if (isMoving)
        {
            animator.SetFloat("XMovement", Vector3.Dot(movementDirection, bodyTransform.right), animationDampTime, Time.deltaTime);
            animator.SetFloat("ZMovement", Vector3.Dot(movementDirection, bodyTransform.forward), animationDampTime, Time.deltaTime);
        } else
        {
            animator.SetFloat("XMovement", 0, animationDampTime, Time.deltaTime);
            animator.SetFloat("ZMovement", 0, animationDampTime, Time.deltaTime);
        }
        
    }
}
