using System;
using System.Collections;
using System.Collections.Generic;
using Unity.Netcode;
using UnityEngine;
using UnityEngine.AI;

public class BearAnimations : NetworkBehaviour
{
    [SerializeField] private Animator animator;

    private Bear bear;

    [SerializeField] private float animationDampTime = 0.1f;

    public override void OnNetworkSpawn()
    {
        if (IsServer)
        {
            bear = GetComponent<Bear>();
            bear.OnAttack += OnAttack;
            bear.OnDeath += OnDeath;
            bear.Health.GetHealthNetworkVar().OnValueChanged += OnHealthChanged;
        }

    }

    public override void OnNetworkDespawn()
    {
        if (IsServer)
        {
            bear.OnAttack -= OnAttack;
            bear.OnDeath -= OnDeath;
            bear.Health.GetHealthNetworkVar().OnValueChanged -= OnHealthChanged;
        }
    }

    private void Update()
    {
        if (!IsServer) return;
        if (!IsSpawned) return;

        // Moving
        Vector3 velocity = bear.GetVelocity();
        Vector3 movementDirection = velocity;
        Transform bodyTransform = transform;
        bool isMoving = velocity.magnitude > 0.1f;

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
        animator.SetBool("Sleeping", bear.GetState() == Bear.State.Sleeping);
    }

    private void OnAttack()
    {
        OnAttackClientRPC();
    }

    [ClientRpc]
    private void OnAttackClientRPC()
    {
        animator.SetTrigger("Attack");
    }

    private void OnDeath()
    {
        OnDeathClientRPC();
    }

    [ClientRpc]
    private void OnDeathClientRPC()
    {
        animator.SetTrigger("Dead");
    }

    private void OnHealthChanged(int oldHealth, int newHealth)
    {
        if (newHealth < oldHealth)
        {
            OnHitClientRPC();
        }
    }

    [ClientRpc]
    private void OnHitClientRPC()
    {
        animator.SetTrigger("Hit");
    }
}
