# 📊 Airbnb Review Frequency Modeling  

This project explores statistical modeling techniques to predict Airbnb listing visibility in NYC, measured by **reviews per year**. The analysis spans five parts: exploratory data analysis, regression assumption checks, interaction modeling, model refinement, and variable selection. The final model uses **weighted least squares** to meet regression assumptions while maintaining interpretability.

---

## 📁 Project Structure

| Part   | Title                                   | Focus                                                                 |
|--------|-----------------------------------------|-----------------------------------------------------------------------|
| Part 1 | Initial Modeling                        | Data exploration and base linear regression with 5 predictors         |
| Part 2 | Assumption Checks                       | Normality, linearity, homoscedasticity, and influence diagnostics     |
| Part 3 | Interaction Effects                     | Pairwise interactions between numerical and categorical variables     |
| Part 4 | Model Refinement                        | Influential point removal, weighted least squares                     |
| Part 5 | Full Model + Variable Selection         | Collinearity checks, model selection, and final WLS model             |

---

## Key Variables

- **Response Variable**: `reviews_per_month` (scaled to `reviews_per_year`)
- **Predictors Considered**:
  - `minimum_nights`
  - `availability_365`
  - `number_of_reviews`
  - `calculated_host_listings_count`
  - `price`
  - `neighbourhood_group` (Brooklyn, Manhattan, Queens, etc.)
  - `room_type` (Entire home, Private room, Shared room)

---

## Part 1: Initial Linear Model

A multiple linear regression was fitted using 5 predictors. Key findings:

- R²: 0.392
- Significant predictors: `minimum_nights`, `availability_365`, `number_of_reviews`
- Non-significant: `price`, `host_listings`

```r
reviews_per_year = 8.96 
                 - 0.12 * minimum_nights
                 + 0.01 * availability_365
                 + 0.22 * number_of_reviews
```

---

## 🔍 Part 2: Diagnostic Checks

- Normality violated (QQ plot + Shapiro-Wilk p < 2.2e-16)
- Linearity questionable, residuals non-randomly scattered
- Homoscedasticity violated, confirmed by scale-location plot and NCV test
- Influential points detected:
  - High leverage: observations like 39876, 870
  - Outliers: standardized residuals > ±3
- Used Cook’s Distance, DFFITS, DFBETAS to assess influence

---

## Part 3: Interaction Modeling

- Included pairwise interactions between:
  - `minimum_nights`, `number_of_reviews`
  - `neighbourhood_group`, `room_type`
- Significant interactions:
  - `minimum_nights:room_typePrivate room`
  - `neighbourhood_group:room_typePrivate room`
- Interpretation:
  - Minimum nights has a positive effect on reviews for private rooms
  - The impact of room type varies by borough (e.g., shared room in Manhattan has higher review frequency than private)

---

## Part 4: Refitting & Weighted Regression

### Base Model:
```r
lm(reviews_per_year ~ minimum_nights + availability_365 + number_of_reviews + neighbourhood_group)
```
- R²: 0.4015  
- Normality and homoscedasticity still violated  
- Influential point: 39876 (Cook’s D = 1.61)

### Final WLS Model (weighted by `minimum_nights²`):
```r
lm(reviews_per_year ~ minimum_nights + availability_365 + number_of_reviews + neighbourhood_group, weights = minimum_nights^2)
```
- R²: 0.953  
- Adjusted R²: 0.9531  
- Homoscedasticity and linearity improved significantly

---

## Part 5: Full Model & Variable Selection

### Collinearity Check:
- VIFs all < 5 → no multicollinearity

### Model Selection:
- forward, backward, stepwise selection methods all suggested to following variables were important to include in model:
  - `minimum_nights`, `availability_365`, `number_of_reviews`, `neighbourhood_group`
- Final WLS Model (weighted by `minimum_nights²`):
```r
lm(reviews_per_year ~ minimum_nights + availability_365 + number_of_reviews + new_neighbourhood_group, weights = minimum_nights^2)
```
- R²: 0.9534  
- Adjusted R²: 0.9531  
- Validated by NCV test (p = 0.16)

---

## 🛠 Tools Used

- **Language**: R  
- **Key Libraries**: `car`, `olsrr`, `lmtest`, `MASS`, `multcomp`  
- **Methods**:  
  - OLS & WLS regression  
  - Residual & influence diagnostics  
  - Variable selection (stepwise, Cp, adjusted R²)  
  - Collinearity diagnostics (VIF, condition number)
