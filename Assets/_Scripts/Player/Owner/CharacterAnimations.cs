using System;
using System.Collections;
using System.Collections.Generic;
using Unity.Netcode;
using UnityEngine;

public class CharacterAnimations : NetworkBehaviour
{
    [SerializeField] private Animator animator;

    [SerializeField] private float animationDampTime = 0.1f;

    public override void OnNetworkSpawn()
    {
        if (IsOwner)
        {
            WeaponManager.OnAttack += OnAttack;
            Character.Local.Health.OnDeath.AddListener(OnDeath);
        }
    }

    public override void OnNetworkDespawn()
    {
        if (IsOwner)
        {
            WeaponManager.OnAttack -= OnAttack;
            Character.Local.Health.OnDeath.RemoveListener(OnDeath);
        }
    }

    private void Update()
    {
        if (!IsSpawned) return;
        if (!IsOwner) return;

        // Movement
        Vector3 movementDirection = Character.Local.MovementDirection;
        Transform bodyTransform = Character.Local.BodyTransform;
        bool isMoving = Character.Local.IsMoving;

        if (isMoving)
        {
            animator.SetFloat("XMovement", Vector3.Dot(movementDirection, bodyTransform.right), animationDampTime, Time.deltaTime);
            animator.SetFloat("ZMovement", Vector3.Dot(movementDirection, bodyTransform.forward), animationDampTime, Time.deltaTime);
        } else
        {
            animator.SetFloat("XMovement", 0, animationDampTime, Time.deltaTime);
            animator.SetFloat("ZMovement", 0, animationDampTime, Time.deltaTime);
        }

        // Weapon
        animator.SetBool("WeaponState", WeaponManager.Local.GetWeaponState());
    }

    private void OnAttack(int combo)
    {
        animator.SetInteger("Combo", combo);

        OnAttackServerRPC();
    }

    [ServerRpc(RequireOwnership = true)]
    private void OnAttackServerRPC()
    {
        ReplicateAttackClientRPC();
    }

    [ClientRpc]
    private void ReplicateAttackClientRPC()
    {
        animator.SetTrigger("Attack");
    }

    private void OnDeath()
    {
        OnDeathServerRPC();
    }

    [ServerRpc(RequireOwnership = true)]
    private void OnDeathServerRPC()
    {
        ReplicateDeathClientRPC();
    }

    [ClientRpc]
    private void ReplicateDeathClientRPC()
    {
        animator.SetTrigger("Death");
    }
}
