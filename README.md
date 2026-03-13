# SPARG — Polymer Chain Structure Generator

**SPARG** (from Portuguese *esparguete*, "spaghetti") is a Fortran 90 mesoscopic simulation program that builds statistical databases of polymer chain configurations. It generates realistic spatial arrangements of polymer chains within a defined volume, subject to physical constraints such as minimum inter-monomer distance, chain-length distributions, and orientation constraints.

**Author:** Ricardo Mendes Ribeiro
**Created:** March 2003 (last updated November 2008)
**Language:** Fortran 90
**Companion tool:** `spagheti.c` — OpenGL 3D visualizer for the output

---

## Table of Contents

- [Overview](#overview)
- [Build Instructions](#build-instructions)
- [Running the Program](#running-the-program)
- [Input File Format (`sparg.ini`)](#input-file-format-spargini)
- [Output Files](#output-files)
- [Filling Strategies](#filling-strategies)
- [Domain Configuration](#domain-configuration)
- [Algorithms](#algorithms)
- [3D Visualizer (`spagheti.c`)](#3d-visualizer-spaghetic)

---

## Overview

SPARG places polymer chains inside a rectangular simulation box, divided into one or more **domains** with distinct physical properties (orientation, density, chain-length distribution). Each chain is built by growing monomers stepwise from a random seed position and orientation. The program avoids overlaps through collision detection, enforces periodic boundary conditions in x and y, and outputs per-sample monomer coordinates together with aggregate statistics across multiple samples.

Typical use case: generating input configurations for neutron/X-ray scattering analysis or mesoscopic molecular dynamics simulations.

---

## Build Instructions

### Fortran simulator (`sparg`)

Requires **gfortran** (GCC Fortran compiler). No external libraries needed.

```bash
# Step 1: concatenate source files in dependency order
cat global.f90 comum.f90 gauss.f90 encaixa.f90 cresce.f90 inputs.f90 print.f90 rotinas.f90 sparg.f90 > arranque.f90

# Step 2: compile
gfortran -O3 -o sparg arranque.f90
```

The helper scripts do the same:

```bash
./juntar    # concatenates all .f90 files into arranque.f90
```

Then replace the `ifc` call in `compilar` with `gfortran -O3 -o sparg arranque.f90`.

### 3D Visualizer (`spagheti`)

Requires OpenGL, GLU, and GLUT development libraries.

```bash
# Ubuntu / Debian
sudo apt install freeglut3-dev libglu1-mesa-dev

gcc -O2 -o spagheti spagheti.c -lGL -lGLU -lglut -lm
```

---

## Running the Program

SPARG reads all parameters from a configuration file named **`sparg.ini`** in the current working directory. No command-line arguments are accepted. If the file is missing the program prints a clear error and stops.

```bash
./sparg
```

The program writes a log to `sparg.txt` and, for each sample run, writes `cadeias.txt` and `monomeros.txt` to the appropriate output directory. Output subdirectories (`./1/`, `./2/`, …) are created automatically when `VEZES` is greater than 1.

---

## Input File Format (`sparg.ini`)

The input file uses a **keyword-based format**. Each keyword occupies one line; the data for that keyword follows on the next line(s). Keywords are case-insensitive. The file must end with `END`.

### Keyword Reference

---

#### `TITLE`
**Description:** Simulation title, written as a header in the log file.
**Format:** One free-text line (up to 80 characters).

```
TITLE
My polymer sample simulation
```

---

#### `OPCOES`
**Description:** Two-character string selecting the filling strategy.
- **First character** controls chain-size assignment (see [Filling Strategies](#filling-strategies)):
  - `A` — largest-chain-first
  - `B` — random order
- **Second character** controls domain construction order:
  - `A` — all domains simultaneously
  - `B` — first two domains first, then the rest
  - `C` — domains filled sequentially in input order

```
OPCOES
BA
```

---

#### `MASSA MONOMERO`
**Description:** Mass of a single monomer in kg.
**Format:** One real number.

```
MASSA MONOMERO
1.38e-25
```

---

#### `DIMENSOES`
**Description:** Dimensions of the simulation box in nm (x, y, z).
**Format:** Three real numbers on one line.

```
DIMENSOES
50.0  50.0  30.0
```

---

#### `DOMINIOS`
**Description:** Defines the number of spatial domains and their properties.
**Format:**

```
DOMINIOS
<opcdom3>          ! 0 = domains grow around seed chains; 1 = explicit boxes
<ndom>             ! number of domains (if opcdom3 ≠ 0)
```

For each domain (repeated `ndom` times):

```
<x_min>  <x_max>                   ! domain x limits (nm)
<y_min>  <y_max>                   ! domain y limits (nm)
<z_min>  <z_max>                   ! domain z limits (nm)
<mean_length>  <std_length>        ! chain length: Gaussian mean & std dev (monomers)
<theta_z>  <delta_z>               ! orientation angle with z-axis: mean & tolerance (degrees)
<theta_x>  <delta_x>               ! orientation angle with x-axis: mean & tolerance (degrees)
<theta_xy>  <delta_xy>             ! orientation angle of xy projection: mean & tolerance (degrees)
<density>                          ! domain density in g/cm³
```

**Example** (two domains):

```
DOMINIOS
1
2
0.0  50.0
0.0  50.0
0.0  15.0
8.0   2.0
45.0  20.0
0.0   90.0
0.0   180.0
0.95
0.0  50.0
0.0  50.0
15.0  30.0
5.0   1.5
90.0  20.0
0.0   90.0
0.0   180.0
0.85
```

---

#### `COMPRIMENTO`
**Description:** Length of one monomer along its axis, in nm.
**Format:** One real number.

```
COMPRIMENTO
0.252
```

---

#### `RAIO`
**Description:** Minimum allowed distance between monomers of different chains, in nm (controls packing density / steric exclusion radius).
**Format:** One real number.

```
RAIO
0.45
```

---

#### `VEZES`
**Description:** Number of independent samples to generate. Each sample is written to its own subdirectory (`./1/`, `./2/`, …), which is created automatically. If `VEZES` is 1 the output is written to `./`.
**Format:** One integer.

```
VEZES
5
```

---

#### `SEED`
**Description:** Integer seed for the random number generator. Using the same seed with the same parameters reproduces identical results.
**Format:** One integer.

```
SEED
12345
```

---

#### `TENTATIVAS DE ENCAIXE`
**Description:** Maximum number of failed placement attempts for a single chain before the program either skips the chain or reduces the target count for that domain.
**Format:** One integer.

```
TENTATIVAS DE ENCAIXE
500
```

---

#### `END`
**Description:** Marks the end of the input file. Required.

```
END
```

### Minimal `sparg.ini` Example

```
TITLE
Test run
OPCOES
BA
MASSA MONOMERO
1.38e-25
DIMENSOES
50.0  50.0  30.0
DOMINIOS
1
1
0.0  50.0
0.0  50.0
0.0  30.0
7.0  2.0
45.0  30.0
0.0  90.0
0.0  180.0
0.92
COMPRIMENTO
0.252
RAIO
0.45
VEZES
1
SEED
42
TENTATIVAS DE ENCAIXE
200
END
```

---

## Output Files

### Per-sample outputs (in `./`, `./1/`, `./2/`, …)

| File | Contents |
|------|----------|
| `cadeias.txt` | One line per chain: domain index, starting monomer index, and all monomer indices in the chain |
| `monomeros.txt` | One line per monomer: `id  x  y  z  alfa  beta  gama` (coordinates in nm, angles in radians) |

### Summary log

| File | Contents |
|------|----------|
| `sparg.txt` | Input echo, domain volumes, estimated and actual chain counts, per-domain statistics for each sample: chain-length histogram, angle distribution (5° bins), z-position distribution |

---

## Filling Strategies

SPARG offers two top-level strategies, selected by the first character of `OPCOES`:

### Option A — Largest-chain-first (`opcdom1 = 0`)

1. Pre-assigns all chain lengths by sampling the Gaussian distribution for each domain.
2. Sorts chains from longest to shortest using an insertion sort.
3. Attempts to place chains in descending size order, randomly selecting their position and orientation within the target domain.

This mimics physical systems where longer chains occupy space first, forcing shorter chains into remaining gaps.

### Option B — Random order (`opcdom1 = 1`)

Chain lengths are drawn on-the-fly during placement. Chains are placed in the order they are attempted. Three sub-options (second character of `OPCOES`) control which domains are filled first:

| Sub-option | Behaviour |
|------------|-----------|
| `A` | All domains are filled simultaneously; each step randomly picks a domain. |
| `B` | Domains 1 and 2 (e.g., crystalline layers) are filled to capacity first, then the remaining domains are filled simultaneously. |
| `C` | Domains are filled sequentially in the order they appear in `sparg.ini`. |

---

## Domain Configuration

Each domain is a rectangular sub-box within the simulation volume. Its key parameters:

| Parameter | Meaning |
|-----------|---------|
| x/y/z limits | Bounding box of the domain in nm |
| `mean_length`, `std_length` | Gaussian parameters for chain length in monomers |
| `theta_z ± delta_z` | Allowed range for the angle between the chain axis and the z-axis (degrees) |
| `theta_x ± delta_x` | Allowed range for the angle between the chain axis and the x-axis (degrees) |
| `theta_xy ± delta_xy` | Allowed range for the projection of the chain axis onto the xy-plane (degrees) |
| `density` | Mass density of the domain in g/cm³, used to estimate the target number of chains |

The program converts all angles to radians internally and derives the target chain count from `density × volume / (monomer_mass × mean_length)`.

---

## Algorithms

### Chain growth (`cresce` module)

Starting from a seed position `(x, y, z)` and orientation angles `(α, β, γ)`:

1. Monomers are placed one by one in the **positive direction** along the chain axis:
   ```
   pos(i) = pos(i-1) + comprimento × (sin α cos γ,  sin α sin γ,  cos α)
   ```
2. If fewer than `tamanho/2` monomers fit in the positive direction (due to boundary or collision), the chain also grows in the **negative direction** to try to reach the target length.
3. The chain is accepted if both halves together reach the requested length; otherwise a failure flag (`-100`) is returned.

### Collision detection (`encaixar` module)

For each candidate monomer position, the squared Euclidean distance to every existing monomer (including periodic images) is computed. The position is accepted only if all distances exceed `raio²`.

### Boundary conditions (`condfronteira` in `rotinas` module)

After each sample is built, x and y coordinates are wrapped into `[0, dimx]` and `[0, dimy]` (periodic/toroidal boundary). The z-axis has hard walls: chains are terminated if they would exit `[0, dimz]`.

### Statistics (`estatisticas` in `rotinas` module)

Histograms are accumulated per domain per sample:
- **Chain-length distribution:** bins 0 to `tam_max_cad`, where the upper bound is computed automatically from the domain parameters as `CEILING(max_mean + 4 × max_std_dev)`.
- **Angle distribution:** bins of 5° from 0° to 90° (angle of chain axis with z).
- **z-position distributions:** 1 nm resolution bins for both chains and monomers.

---

## 3D Visualizer (`spagheti.c`)

A companion OpenGL/GLUT C program for interactive visualization of the `monomeros.txt` output. It reads `sparg.ini` automatically to set the simulation box dimensions and monomer length; if `sparg.ini` is absent it falls back to default values (15 × 15 × 100 nm, monomer length 0.625 nm).

Monomer arrays are allocated dynamically, so there is no upper limit on the number of monomers that can be visualized.

### Compilation

```bash
gcc -O2 -o spagheti spagheti.c -lGL -lGLU -lglut -lm
```

### Usage

Run from the directory that contains `monomeros.txt` and `sparg.ini`:

```bash
./spagheti
```

### Controls

| Key / Action | Effect |
|--------------|--------|
| Mouse drag | Rotate the view |
| Arrow Up / Down | Translate up / down |
| Arrow Left / Right | Translate left / right |
| `+` / `-` | Translate along z |
| `z` / `x` | Zoom in / out |
| `q` | Quit |

Monomers are rendered as line segments color-coded by orientation angle (darker = more aligned with z). The simulation box boundary is drawn as a white wireframe scaled to the actual `DIMENSOES` from `sparg.ini`.
