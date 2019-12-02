# BayesianNetwork
Shiny application for Exposure based modelling using Bayesian network.

#### Exposure based modes:
The Basel Committee has dismissed statistical approaches to risk modelling, leaving regulators and practitioners searching for the 
next generation of OpRisk quantification. The XOI method is ideally suited to fulfil this need, as a calculated, coordinated, 
consistent approach designed to bridge the gap between risk quantification and risk management. This exposure based approach to 
OpRisk modelling with a proven, non-statistical methodology Operational Risk Modeling in financial services provides risk professionals
with a forward-looking approach to risk modelling, based on structured management judgement over obsolete statistical methods. 
This method is well suited for major operational risks, such as disasters, fraud, conduct, legal and cyber risk etc.

In another words, this approach focuses on capturing the forward-looking view of the risk, and understanding what factors drive 
the risk. The approach is new and refreshing because it avoids the reflex reaction to use historical loss data (how much was lost) 
as the primary input into the quantification process. Rather, it first decomposes the risk into the risk factors that influence the 
level of risk, and then looks to calibrate those factors using the best available information. If the factor in question is observable, 
then data can be used to calibrate, otherwise Subject Matter expert’s estimates can be used. The XOI method for quantifying operational 
risk is a very flexible and adaptable framework.

#### How it works:
Exposure based model works on Bayesian network framework. This type of model provide intuitive representation of the key associated 
factors depend on data and expert judgement. In short, this is a probabilistic graphical model based on conditional dependence. 
To give an overview on Bayesian network, it is a set of nodes each containing a probability distribution that describes and quantifies 
the uncertainty in a process or event. The conditional dependence of the nodes is explicitly represented through directed edges 
(which means a node can only be a child of a parent node or it can be top level parent node). Having said that, two-way edges are 
not allowed i.e. a child node can’t affect its parent or grandparent’s nodes in any direct or indirect way, The child is affected by 
parent(s) only, not vice versa. The directed edges represent causal processes i.e. how an event influence another event. In short, 
it is also be called XOI model. X is for exposure at risk for e.g. it can be number of buildings/ applications etc. O or occurrence 
is probability of an event. I or impact is the actual loss that could occur given that event has already happened.

#### Advantages:
-	Requires very less or no data in which case, the model rely solely on SME’s input
-	It’s forward looking
-	The network can be visually observed and it’s interpretable
-	Simple and easy to understand the risk factors

#### Disadvantages:
-	The model requires SME’s input which may subject to uncertainty leading to model uncertainty. 
-   If the network become too large, it will be difficult to understand and interpret.

