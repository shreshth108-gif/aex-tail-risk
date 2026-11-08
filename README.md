# Heavy Tails and Value-at-Risk in 30 Years of AEX Returns

Statistical modelling of AEX daily returns from 1994 to 2023, covering distribution fitting, one-day Value-at-Risk and Monte Carlo confidence intervals.

**Mathematical Statistics · Aug - Oct 2024 · Grade 9.0/10**

<!-- REPORT_LINK -->

## Questions

- How heavy are the tails of AEX daily returns, and is an i.i.d. model reasonable?
- How large is the one-day 1% Value-at-Risk, and how much does it depend on the model?
- Is the mean daily return significantly different from zero?
- Does "Sell in May and go away" hold up in the data?

## Approach

- Exploratory analysis of daily prices and returns over 30 years, through the dot-com crash, the 2008 crisis and COVID-19.
- For 2004 and 2019, a custom sample autocorrelation function, compared with the built-in one, to test temporal independence.
- A Laplace (double-exponential) model fitted by maximum likelihood to capture the sharp central peak and heavy tails.
- 1% VaR from the closed-form Laplace quantile $q_p = \mu + \sigma\log(2p)$, reported as the loss $-q_p$, compared with the empirical 1% quantile.
- 95% confidence intervals for the location parameter from the pivotal quantity $Q = (\hat\mu - \mu)/\hat\sigma$. Its distribution has no closed form, so its quantiles are simulated by Monte Carlo.

## Results

| Year | 1% VaR (Laplace) | 1% VaR (empirical) | 95% CI for $\mu$ |
|---|---|---|---|
| 2004 | 2.26% | 2.06% | $[-1.23,\ 1.30] \times 10^{-3}$ |
| 2019 | 1.61% | 1.66% | $[0.17,\ 1.77] \times 10^{-3}$ |

- The Laplace model gives the higher VaR in 2004 (2.26% vs 2.06%) but a slightly lower one in 2019 (1.61% vs 1.66%), so whether the parametric model is the conservative choice depends on the year.
- The 2004 interval contains zero but the 2019 interval does not, so the 2019 daily return (median and mean coincide under the Laplace model) was significantly positive.
- Summer returns give no clear support for "Sell in May". Their mean is slightly negative, but large gains are as likely as large losses.

> Note: the original report lists a 2019 Laplace VaR of 1.786%. Recomputing it from the report's own estimates ($\hat\mu = 9.699 \times 10^{-4}$, $\hat b = 4.372 \times 10^{-3}$) gives 1.61%, and this README uses the corrected value. The 2004 figure reproduces.

## Repository contents

- `src/question_1_exploratory_analysis.R`: prices, daily returns and their distribution, and the summer-versus-rest-of-year comparison
- `src/question_2_inference.R`: the custom ACF, the Laplace fit, parametric and empirical VaR, and the Monte Carlo confidence intervals

The scripts expect `data/AEX19942023.csv`, which is not included (`data/` is git-ignored). `read_csv` comes from the `readr` package, so load it first.

**Tools:** R
