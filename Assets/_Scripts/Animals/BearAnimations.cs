using System;
using System.Collections;
using System.Collections.Generic;
using Unity.Netcode;
using UnityEngine;
using UnityEngine.AI;

public class BearAnimationController : NetworkBehaviour
{
    [SerializeField] private Animator animator;
    [SerializeField] private NavMeshAgent agent;
    [SerializeField] private BearController controller;

    [SerializeField] private float animationDampTime = 0.1f;

    public override void OnNetworkSpawn()
    {
        if (IsServer)
        {
            controller.OnAttack += OnAttackServer;
        }
    }

    private void Update()
    {
        if (!IsServer) return;
        if (!IsSpawned) return;

        // Moving
        Vector3 movementDirection = agent.velocity.normalized;
        Transform bodyTransform = transform;
        bool isMoving = agent.velocity.magnitude > 0.1f;

        animator.SetBool("Moving", isMoving);

        if (isMoving)
        {
            animator.SetFloat("XMovement", Vector3.Dot(movementDirection, bodyTransform.right), animationDampTime, Time.deltaTime);
            animator.SetFloat("ZMovement", Vector3.Dot(movementDirection, bodyTransform.forward), animationDampTime, Time.deltaTime);
        }
        else
        {
            animator.SetFloat("XMovement", 0, animationDampTime, Time.deltaTime);
            animator.SetFloat("ZMovement", 0, animationDampTime, Time.deltaTime);
        }

        // Sleeping
        animator.SetBool("Sleeping", controller.GetState() == BearController.State.Sleeping);
    }

    private void OnAttackServer()
    {
        OnAttackClientRPC();
    }

    [ClientRpc]
    private void OnAttackClientRPC()
    {
        animator.SetTrigger("Attack");
    }
}
