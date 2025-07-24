# Orthogonal Persistence for DApps

## Why Orthogonal Persistence for DApps?

### DApps need Persistence
Decentralized Applications (DApps) have both on-chain and off-chain components.
While the simplest DApps may store all relevant data on-chain,
many advanced DApps crucially depend on off-chain state not to lose fund:
for instance, applications may use State Channels (using Hydra on Cardano)
to enable more transactions, faster, wherein all intermediate states remain completely private;
however, if some participant fails to remember the latest state of the State Channel,
they may lose all funds earned since the last state they remember.
Thus, if partaking in some complex multi-leg DeFi swap one forgets his status mid-trade,
one may lose all the stake on the table.
Worse, failure to properly implement persistence opens a user to attacks
by malicious participants they deal with, who might then be able to steal assets from the user.

### Manual Persistence is Hard
Robust persistence of data is therefore of the utmost importance for anything but the simplest DApps.
But robust persistence is hard, especially if it has to be achieved manually:
the application must make sure to checkpoint the state regularly, and
especially before to send messages to a blockchain or to another DApp participant;
the infrastructure must make sure to correctly encrypt and replicate the state across
many data centers, which would crucially depend on each user's configuration;
and whenever the application or the user configuration
or the blockchain or storage protocol changes the least,
all the persistence code must change accordingly without fail.
This is typically too much effort for any small DApp developer, or isolated user,
and requires a blockchain-wide solution, a cross-blockchain solution even.

### Orthogonal Persistence can Automate Persistence
Happily, an old technology recently made new again can automatically solve persistence problems,
with almost no involvement from DApp programmers, and little involvement from users
beside configuring their storage providers and paying these providers' annual fees:
Orthogonal Persistence, a technique mastered in the 1980s-1990s, forgotten in the 2000s,
and recently made popular again under the moniker "Durable Execution"
by vendors of "orchestration" software.

But while the current vendors of "Durable Execution" orchestrators provide products fit for
large complex applications backed by a full-time development and system administration team,
they are not fit to be used for DApps: DApps must be radically simple so they can be audited;
they cannot afford to have complex APIs for task restart or SQL encoding of data.
They must support the on-chain and off-chain languages of DApps, and
run with end-users for whom paying yearly fees to a few backup vendors they have to choose from
is the most system administration they can do.

Thus, we need some kind of virtual machine specialized for off-chain DApp execution,
that automatically persists all the data “orthogonally” to the concerns of the DApp programmers
(i.e. DApp programmers don't have to worry about it),
and according to the configuration of the user.

### A Model for Orthogonal Persistence
We have previously described a
[model for Orthogonal Persistence](https://github.com/mighty-gerbils/gerbil-persist/blob/master/persist.md)
and given a talk about at LambdaConf 2025
([video](https://youtu.be/stEl-RBJVdA),
[slides](http://fare.tunes.org/files/cs/persist/slides-2025-lambdaconf.html)).

This model is not specific to DApps, and could be used for any kind of application;
but this model is especially fit for DApps, and languages for DApps (such as Plutus, Aiken)
could be advantageously modified to run on top of a virtual machine that implements this model:
then, DApp users will be immune to accidental or malevolently-induced failures to persist data.

## Proof of Concept for Orthogonal Persistence for DApps

### General Plan Summary

The plan will be in several steps.
For the current grant proposal, we will only implement step 1;
steps 2 and 3 are left as follow-ups to be funded later after the proof-of-concept is successful:

1. Proof of Concept of Orthogonal Persistence
  - Proof of concept design of a Simple Orthogonally Persistent Virtual Machine (SOPVM)
    fit for off-chain execution of DApp code.
  - Proof of concept implementation of the Simple Orthogonally Persistent Virtual Machine (SOPVM)
    fit for off-chain execution of DApp code.
  - Proof of concept design of a Simple Orthogonally Persistent Language (SOPL)
    fit for off-chain execution of DApp code.
  - Proof of concept implementation of the Simple Orthogonally Persistent Language (SOPL)
    as interpreted or macro-expanded into some bootstrap metalanguage.
  - Proof of concept implementation of the Simple Orthogonally Persistent Language (SOPL)
    as compiled or macro-expanded into the Simple Orthogonally Persistent Virtual Machine (SOPVM),
    implemented in the SOPL itself ("bootstrapped").

2. Proof of Concept Integration SOPL with Smart Contracts
  - Extension of the SOPL runtime so programs can interact with a blockchain.
    Typically, a persistent abstraction over access to various blockchain APIs.
  - Example of a Smart Contract client written in SOPL so it won't lose state.
    The contract itself may be written in Plutus, Aiken, Solidity, etc.;
    but the client is written in SOPL.
  - Test cases demonstrating the utility of persistence.

3. Actual integration of SOPL with Plutus, Aiken or some other contract language.

### Details on Proposed Step 1: Proof of Concept Orthogonal Persistence

We will implement a Simple Orthogonally Persistent Virtual Machine (SOPVM),
and on top of it, a Simple Orthogonally Persistent Language (SOPL).
For everything to remain as simple as possible,
the SOPVM will be based on the applicative untyped (dynamically typed) λ-calculus,
and the SOPL will be based on the programming language Scheme,
whereas the metalanguage will be an existing variant of the programming language Scheme.

The SOPVM will persist data at the level of granularity of the individual language-level value,
i.e. each program-level value or object, will be persistently saved as its own database record.
Evaluation will be divided into transaction blocks;
each transaction will commit all changes to records since the end of the previous transaction.
The evaluator will lock the database and/or the relevant tables for its exclusive writable use,
so it knows exactly which objects have changed.
Communication with the blockchain or other participants blocks until all previous changes
are persisted (i.e. part of a past transaction).

### Features not included in the initial Proof of Concept

The following features will not be included in the initial Proof of Concept,
and will only be implemented after a successful Proof-of-Concept is demonstrated:

- Support for more DApp languages:
  The initial Proof of Concept will only support the SOPL dialect of Scheme.
  A future project will instead support Plutus, Aiken or JavaScript
  as the Orthogonally Persistent Language.

- Support for Remote Backends:
  The initial Proof of Concept will only support local storage on SQLite.
  A future project will additionally support remote storage to corporate cloud
  (Amazon, Google, Azure, etc.) as well as various IPFS providers.

- Database Encryption and Sharding:
  The initial Proof of Concept will only support plain text storage of data.
  A future project will additionally support encrypting all data, and
  using erasure coding such that n-out-of-m storage providers are sufficient and necessary
  to recover all the data.
