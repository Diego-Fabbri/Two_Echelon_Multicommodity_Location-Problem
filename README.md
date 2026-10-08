# Two-Echelon Multicommodity Location Problem (TEMC)

A **Mixed Integer Linear Programming (MILP)** model in **R** for the **Two-Echelon Multicommodity Location Problem**, built with the [`ompr`](https://dirkschumacher.github.io/ompr/) modeling framework and solved via the **SYMPHONY** solver (through `ROI`).

## Overview

The Two-Echelon Multicommodity Location Problem (TEMC) is a strategic supply chain network design problem in Operations Research. Unlike the single-echelon Capacitated Plant Location Problem, the TEMC introduces an **intermediate distribution level** between production plants and final customers:

$$
\text{Production Plants} \longrightarrow \text{Distribution Centers (DCs)} \longrightarrow \text{Demand Nodes}
$$

The model simultaneously decides:
1. **Which DCs to open** — from a set of potential locations, subject to a maximum number of openings
2. **Which DC serves each demand node** — one-to-one assignment of customers to opened DCs
3. **How to route commodity flows** from plants through DCs to demand nodes at minimum cost

The objective minimizes the total of transportation costs, fixed DC opening costs, and variable (marginal) DC operating costs that scale with the volume served.

## Repository Contents

| File | Description |
|---|---|
| `Two-Echelon Multicommodity Location Model.R` | R script implementing and solving the TEMC instance |
| `Two-Echelon Multicommodity Location Model.pdf` | Mathematical formulation of the problem |

## Mathematical Formulation

### Sets

- $I$ = set of production plants (index $i$)
- $J$ = set of potential Distribution Centers (index $j$)
- $R$ = set of demand nodes (index $r$)
- $K$ = set of homogeneous commodities (index $k$)

### Parameters

- $p$ = maximum number of DCs that can be opened
- $c_{kijr}$ = unit transportation cost of commodity $k$ from plant $i$ to demand node $r$ via DC $j$
- $d_{kr}$ = demand for commodity $k$ at demand node $r$
- $p^k_i$ = maximum production capacity of plant $i$ for commodity $k$
- $q_j^-$ = minimum activity level (throughput) of DC $j$
- $q_j^+$ = maximum activity level (throughput) of DC $j$
- $f_j$ = fixed cost of opening DC $j$
- $g_j$ = marginal (variable) cost per unit of throughput at DC $j$

### Feasibility Condition

The problem admits a feasible solution only when total plant capacity covers total demand for each commodity:

$$
\displaystyle \sum_{i \in I} p^k_i \ge \sum_{r \in R} d_{kr} \qquad \forall\, k \in K
$$

### Variables

- $s_{kijr}$ = amount of commodity $k$ transported from plant $i$ to demand node $r$ via DC $j$; $s_{kijr} \ge 0$

$$
z_j = \begin{cases} 1 & \text{if DC } j \in J \text{ is opened} \\ 0 & \text{otherwise} \end{cases}
$$

$$
y_{jr} = \begin{cases} 1 & \text{if demand node } r \in R \text{ is assigned to DC } j \in J \\ 0 & \text{otherwise} \end{cases}
$$

### Objective Function

Minimize total cost: transportation + fixed DC opening + marginal DC operating costs

$$
\displaystyle \min \sum_{i \in I} \sum_{j \in J} \sum_{r \in R} \sum_{k \in K} c_{kijr} \cdot s_{kijr} + \sum_{j \in J} \left( f_j \cdot z_j + g_j \cdot \sum_{r \in R} \sum_{k \in K} d_{kr} \cdot y_{jr} \right)
$$

### Constraints

**(1)** — Plant capacity: total flow from each plant cannot exceed its production capacity

$$
\displaystyle \sum_{j \in J} \sum_{r \in R} s_{kijr} \le p^k_i \qquad \forall\, i \in I,\ k \in K
$$

**(2)** — Flow-assignment linking: commodity shipped to a demand node via a DC must match the assignment decision

$$
\displaystyle \sum_{i \in I} s_{kijr} = d_{kr} \cdot y_{jr} \qquad \forall\, j \in J,\ r \in R,\ k \in K
$$

**(3)** — Each demand node is assigned to exactly one DC

$$
\displaystyle \sum_{j \in J} y_{jr} = 1 \qquad \forall\, r \in R
$$

**(4)** — DC activity bounds: throughput at each open DC must respect minimum and maximum levels

$$
q_j^- \cdot z_j \le \displaystyle \sum_{r \in R} \sum_{k \in K} d_{kr} \cdot y_{jr} \le q_j^+ \cdot z_j \qquad \forall\, j \in J
$$

**(5)** — Exactly $p$ DCs must be opened

$$
\displaystyle \sum_{j \in J} z_j = p
$$

**(6)** — Binary DC opening decision

$$
z_j \in \{0, 1\} \qquad \forall\, j \in J
$$

**(7)** — Binary customer-to-DC assignment

$$
y_{jr} \in \{0, 1\} \qquad \forall\, j \in J,\ r \in R
$$

**(8)** — Non-negative commodity flows

$$
s_{kijr} \ge 0 \qquad \forall\, i \in I,\ j \in J,\ r \in R,\ k \in K
$$

> **Note on the two-echelon structure:** The TEMC extends the single-echelon MCPL by adding an intermediate DC layer and two additional decision variables — $z_j$ (open a DC) and $y_{jr}$ (assign a customer to a DC). Constraint (2) links flows to assignments: if $y_{jr} = 0$, no goods flow from any plant to demand node $r$ via DC $j$. The objective also includes a **marginal cost** $g_j$ per unit of volume processed at each DC, which is absent in the single-echelon model.

> **Decomposition property:** As noted in the PDF, once $z_j$ and $y_{jr}$ are fixed (e.g. by enumeration or heuristic), the optimal commodity flows can be found by solving a pure LP — the demand allocation sub-problem. This structure is often exploited by Lagrangean relaxation or Benders decomposition methods.

A copy of this formulation is also available as a standalone PDF in this repository.

## Example Instance

The script uses a hardcoded (but partially random) instance with **2 plants**, **2 potential DCs**, **2 demand nodes**, and **1 commodity** ($P_\text{max} = 1$ DC to open):

**Plant capacities** (commodity 1):

| Plant $i$ | Capacity $p^1_i$ |
|:---:|---:|
| 1 | 1,200 |
| 2 | 1,500 |
| **Total** | **2,700** |

**Demand nodes** (commodity 1):

| Demand node $r$ | Demand $d_{1r}$ |
|:---:|---:|
| 1 | 800 |
| 2 | 600 |
| **Total** | **1,400** |

- **Feasibility**: total capacity (2,700) > total demand (1,400) ✓

**DC parameters:**

| DC $j$ | Fixed cost $f_j$ | Marginal cost $g_j$ | Min throughput $q_j^-$ | Max throughput $q_j^+$ |
|:---:|---:|---:|---:|---:|
| 1 | 960,000 | 1.00 | 0 | 1,500 |
| 2 | 880,000 | 2.00 | 0 | 1,200 |

**Transportation costs** $c_{kijr}$: randomly generated with `set.seed(12345)` from integer values in $[1, 15]$ plus a random decimal in $[0, 1]$, ensuring full reproducibility.

## Requirements

```r
install.packages(c("lpSolve", "dplyr", "ROI", "ROI.plugin.symphony", "ompr", "ompr.roi"))
```

## Usage

1. Clone or download this repository.
2. Open `Two-Echelon Multicommodity Location Model.R` in R or RStudio.
3. Update the `setwd()` path at the top of the script to match your local directory.
4. Run the script. It will:
   - Check the feasibility condition for each commodity
   - Build and solve the MILP model using `ompr` and SYMPHONY
   - Print the solver status, optimal total cost, open DC decisions $z[j]$, customer-to-DC assignments $y[j,r]$, and commodity flows $s[k,i,j,r] > 0$

## Output

The script prints:

- **Feasibility status** — whether plant capacity covers demand for each commodity
- **Model status** — whether an optimal solution was found
- **Objective value** — the minimum total cost (transportation + fixed + marginal)
- **$z[j]$ variables** — which DCs are opened
- **$y[j, r]$ variables** — which DC serves each demand node
- **$s[k, i, j, r]$ variables** — all non-zero commodity flows from plants to demand nodes via DCs
