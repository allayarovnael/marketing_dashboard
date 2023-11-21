# Marketing Mix Analysis Dashboard 
The following app is written in R and uses R Shiny frontend to demonstrate the typical outputs of a marketing-mix-modeling project. 

## Depolyed version
This app was wrapped into Docker container and deploed using [HuggingFace spaces](https://huggingface.co/spaces/allayarovnael/marketing_app).

## Features
<ul>
  <li>Model fit:
  ![alt text](https://github.com/allayarovnael/marketing_dashboard/blob/main/images/model_fit.png "model_fit")
  </li>
  
  <li>Visualization of impacts:
  ![alt text](https://github.com/allayarovnael/marketing_dashboard/blob/main/images/impacts.png "impacts")
  </li>
  
  <li>Media adstock effects: carry-over and saturation:
  ![alt text](https://github.com/allayarovnael/marketing_dashboard/blob/main/images/media_adstock.png "media_adstock")
  </li>

  <li>Model disgnostics: check of multicollinearity, autocorrealtion, normality and homoscedacity of residulas:
  ![alt text](https://github.com/allayarovnael/marketing_dashboard/blob/main/images/diagnostics.png "diagnostics")
  </li>
</ul>

## Requirements
R libraries:
<ul>
<li>shiny</li>
<li>semantic.dashboard</li>
<li>shiny.semantic</li>
<li>car</li>
<li>highcharter</li>
<li>stargazer</li>
<li>dplyr</li>
<li>tidyr</li>
<li>purrr</li>
<li>forecast</li>
<li>lubridate</li>
<li>broom</li>
<li>openxlsx</li>
</ul>

