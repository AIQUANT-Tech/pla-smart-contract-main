# Plastiks Smart Contracts — `SmartContract` folder

**Repository:** `https://github.com/AIQUANT13/pla-smart-contract.git`


## Overview

This folder contains the Haskell / Plutus smart contract modules used by the Plastiks project. The contracts implement minting, burning, role-based access control, account verification, and additional on-chain logic used by the application. Use this README as a quick reference for the files in the `SmartContract/` directory and basic developer instructions.

---

## Files in this folder

Below are the files visible in the `SmartContract` directory and a short description for each.

* **PCMint.hs** 
(https://github.com/AIQUANT13/pla-smart-contract/blob/main/SmartContract/PCMint.hs)

  * Primary minting contract (Plutus validator or minting policy) for plastic credits/tokens. Entry points for mint operations and on-chain validation logic.

* **PCMintV2.hs**
(https://github.com/AIQUANT13/pla-smart-contract/blob/main/SmartContract/PCMintV2.hs)

  * Second version of the `PCMint` module. Likely contains fixes, improved validation, or an updated datum/redeemer layout and additional tests.

* **PlastikBurner.hs**
(`https://github.com/AIQUANT13/pla-smart-contract/blob/main/SmartContract/PlastikBurner.hs`)

  * Burning policy/validator for retiring tokens or credits. Responsible for validating and authorizing token burn operations.

    #### Test Cases Written: 
  https://github.com/AIQUANT13/pla-smart-contract/blob/main/SmartContractTest/TestPlastikBurner.hs

* **PlastikCryptoV2.hs**
(https://github.com/AIQUANT13/pla-smart-contract/blob/main/SmartContract/PlastikCryptoV2.hs)

  * Cryptographic helpers, signature verification, voucher verification functions and utilities used by other validators. (E.g., verifying seller vouchers, price signatures, or PRG vouchers.)

   #### Test Cases Written: 
  https://github.com/AIQUANT13/pla-smart-contract/blob/main/SmartContractTest/PlastikCryptoV2Test.hs

* **PlastikPRGV3OnChain.hs**
(https://github.com/AIQUANT13/pla-smart-contract/blob/main/SmartContract/PlastikPRGV3OnChain.hs)

  * On-chain portion of a PRG (pseudo-random generator) or PRG-backed voucher/claim logic (version 3). Contains validators that run fully on-chain.

   #### Test Cases Written: 
  https://github.com/AIQUANT13/pla-smart-contract/blob/main/SmartContractTest/TestPlastikPRGV3OnChain.hs

* **PlastikRecoveryProjects.hs**
(https://github.com/AIQUANT13/pla-smart-contract/blob/main/SmartContract/PlastikRecoveryProjects.hs)

  * Validator or helper code to recover or manage project state on-chain. Might include recovery flows or project-level state transitions.

   #### Test Cases Written: 
  https://github.com/AIQUANT13/pla-smart-contract/blob/main/SmartContractTest/TestPlastikRecoveryProjects.hs


* **PlastikRoleV2.hs**
(http://github.com/AIQUANT13/pla-smart-contract/blob/main/SmartContract/PlastikRoleV2.hs)

  * Role-based validator implementing roles and permissions (e.g., `GrantMinter`, `VerifyMinter`). Likely used to manage role assignment on-chain and ensure only authorized actions occur.

   #### Test Cases Written: 
     https://github.com/AIQUANT13/pla-smart-contract/blob/main/SmartContractTest/TestPlastikRoleV2.hs

* **PlastikTokenMint.hs**
(https://github.com/AIQUANT13/pla-smart-contract/blob/main/SmartContract/PlastikTokenMint.hs)

  * Token minting policy that defines token issuance rules, supply constraints and policy scripts used for minting the main project token(s).


* **VerifiedAccounts.hs**
(`https://github.com/AIQUANT13/pla-smart-contract/blob/main/SmartContract/VerifiedAccounts.h)

  * Helpers and on-chain checks for verified account flows — e.g. registry of verified recyclers/accounts, signature checks, or lookups.

  #### Test Cases Written: 
    https://github.com/AIQUANT13/pla-smart-contract/blob/main/SmartContractTest/TestVerifiedAccounts.hs

* **WhiteListSender.hs**
(https://github.com/AIQUANT13/pla-smart-contract/blob/main/SmartContract/WhiteListSender.hs)

  * Utility or validator for performing whitelist-controlled sends, or for maintaining a whitelist of permitted senders/addresses.

  #### Test Cases Written: 
    https://github.com/AIQUANT13/pla-smart-contract/blob/main/SmartContractTest/TestWhiteListSender.hs

---

## Quick start (developer)

These are generic Haskell/Plutus steps. Adapt to your local toolchain (Nix, Cabal, Stack) and to the project’s CI.

1. **Prerequisites**

   * Haskell toolchain (GHC + Cabal or Stack) and Cabal/Stack on PATH
   * Nix (optional) if your project uses Nix for reproducible builds
   * Plutus libraries and dependencies as defined in the project `cabal.project`/`package.yaml`

2. **Clone the repository**

```bash
git clone https://github.com/your-org/your-repo.git
cd your-repo/SmartContract
```

3. **Build** (Cabal)

```bash
cabal build
```

Or with Stack:

```bash
stack build
```

If using Nix, follow the repo’s `README` for `nix-shell` and build instructions.

4. **Run tests**

* Unit tests and property tests (Tasty + QuickCheck) are typically run via:

```bash
cabal test
# or
stack test
```

5. **Generate `.plutus` files / compile validators**

* Many Plutus projects expose a `plutus` or `scripts` target to produce serialized validator files. Check the repo’s build scripts or `Makefile`.

6. **Local REPL / debugging**

```bash
cabal repl
# then import modules: :l PCMint
```

---

## Testing & CI recommendations

* Keep QuickCheck-based property tests (as you have been doing) for critical invariants: signature checks, datum format, duplicate ID checks, reputation calculations, role grants, and registry updates.
* Use Tasty to group unit tests and to run deterministic test cases for both happy and failure paths.
* Add a CI job to compile all modules and run tests on pushes and PRs.

---

## Security & deployment notes

* Keep private keys, signing keys, and KMS secrets out of source control. Use Google Secret Manager / AWS Secrets Manager / HashiCorp Vault.
* Validate datums and redeemers strictly on-chain; prefer inline datums for state flows where appropriate.
* Use a staging/testnet flow and automated integration tests before any mainnet deploy.

---

## Contributing

1. Fork the repo and create a feature branch.
2. Run tests locally and ensure new code is covered by tests.
3. Open a PR with a clear description and link to any design notes.


