## Build neural networks from square one in R

This is a guide on building neural network from ground up in `R`. The focus is 
on understanding how NN works and how the algorithms used for NN assemble into a
model.

There are **4** parts in this project. We start from
[logistic regression](logistic_regression_nn.R) network -- this is the most
simple model. Then we build a
[simple network](simple_nn.R) with 3 layers -- or 1 hidden layer. This part
shows how different elements of neural network assemble into a feed forward
network. Part **3** of the project shows how to implement
[multi-layer](nn_step_by_step.R) feed forward network without optimization.
Part **4** of the project shows how to train [l-layer network](train_l_layer.R) 
with elements of optimization:

- initialization methods: standard, random, He  
- optimization algorithms: gradient descent, Adam  
- regularization techniques: L2, drop out  
- mini-batch optimization  


## Details on computations

There are **2** critical computation methods that we focus our attention on.
The first one belongs to *activation* functions computations -- there is an
alternative way to `ifelse()` desribed [here](./readme_files/activations_readme.Rmd)
We also propose to use `eval()` function in order to be able to compute as many
layers of neural network as we need. The approach is described
[here](./readme_files/regularization_cost.Rmd)


## Sources

Sources used in this project:

- [Deep Learning](https://www.coursera.org/specializations/deep-learning)  
- [Rstudio blog part 1](https://rviews.rstudio.com/2020/07/20/shallow-neural-net-from-scratch-using-r-part-1/)  
- [Rstudio blog part 2](https://www.r-bloggers.com/2020/07/building-a-neural-net-from-scratch-using-r-part-2/)  

