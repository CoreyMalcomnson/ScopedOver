using System.Collections;
using System.Collections.Generic;
using Unity.Netcode;
using UnityEngine;

public class CharacterAnimationController : NetworkBehaviour
{
    [SerializeField] private Animator animator;

    [SerializeField] private float animationDampTime = 0.1f;

    public override void OnNetworkSpawn()
    {
        if (!IsOwner)
        {
            this.enabled = false;
            return;
        }
    }

    private void Update()
    {
        Vector3 movementDirection = CharacterMovementController.Local.MovementDirection;
        Transform bodyTransform = CharacterMovementController.Local.BodyTransform;
        bool isMoving = CharacterMovementController.Local.IsMoving;

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
