*Jiahao Tian*

### Overall Grade: 160/180

### Quality of report: 10/10

-   Is the homework submitted (git tag time) before deadline? 

-   Is the final report in a human readable format html? 

-   Is the report prepared as a dynamic document (R markdown) for better reproducibility?

-   Is the report clear (whole sentences, typos, grammar)? Do readers have a clear idea what's going on and how are results produced by just reading the report? 

### Completeness, correctness and efficiency of solution: 100/120

- Q1 (10/10)

- Q2 (10/10)

- Q3 (20/20)

- Q4 (5/10)

    Did not only keep the patients who have a match in icustays_tble (according to subject_id). Use semi-join with icustays_tble, by = 'subject_id'. `-5`.

- Q5 (15/20)

    You did a great job here, but just missed %>% pivot_wider(). This makes it one row per person, with each column representing a lab event. joins and pivot_wider() in Q4 above and have a dataset with 53150 rows, one per person. `-5`.

- Q6 (15/20)

    Same as Q5. `-5`.

- Q7 (15/20)

    If you fix Q4,5,6, then you will get the right dataset. You also have to include the filter for each unique adult (age at admission > 18). The right dataset has 53065 rows and roughly 42-44 columns depending on how you filter for age at admission. `-5`.

- Q8 (10/10)

	    
### Usage of Git: 10/10

-   Are branches (`master` and `develop`) correctly set up? Is the hw submission put into the `master` branch?

    Yes
  
-   Are there enough commits? Are commit messages clear? 
  
    Yes
          
-   Is the hw2 submission tagged? 

    Yes
  
-   Are the folders (`hw1`, `hw2`, ...) created correctly? 
  
    Yes
  
-   Do not put a lot auxiliary files into version control. If any unnecessary files are in Git, take 5 points off.

### Reproducibility: 10/10

This HW might be difficult to check reproducibility. 

-   Are the materials (files and instructions) submitted to the `master` branch sufficient for reproducing all the results? Just click the `knit` button will produce the final `html` on teaching server? 

    Yes
  
-   If necessary, are there clear instructions, either in report or in a separate file, how to reproduce the results?

### R code style: 20/20

Each violation takes 2 points off, until all 20 points are depleted.

-   [Rule 2.5](https://style.tidyverse.org/syntax.html#long-lines) The maximum line length is 80 characters.  

-   [Rule 2.4.1](https://style.tidyverse.org/syntax.html#indenting) When indenting your code, use two spaces.  

-   [Rule 2.2.4](https://style.tidyverse.org/syntax.html#infix-operators) Place spaces around all infix operators (=, +, -, &lt;-, etc.).  

-   [Rule 2.2.1.](https://style.tidyverse.org/syntax.html#commas) Do not place a space before a comma, but always place one after a comma.  

-   [Rule 2.2.2](https://style.tidyverse.org/syntax.html#parentheses) Do not place spaces around code in parentheses or square brackets. Place a space before left parenthesis, except in a function call.
