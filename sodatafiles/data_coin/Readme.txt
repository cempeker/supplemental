############################


The text below is adapted from the readme file in Palley and Soll (2019) on the Coin Flips data. See the supplemental material available at https://pubsonline.informs.org/doi/suppl/10.1287/mnsc.2018.3047  


################################


Study 1 Variables. Note Study 1 responses are stored in a “wide” format, so that responses for all 8 coins for a participant are listed in a single row in the csv file.

SubjectID: unique participant identifier

Condition: keeps track of the information setting, with Condition=1 for Symmetric; Condition=2 for Nested-Symmetric; Condition=3 for Nested	

minutes: number of minutes elapsed from start to finish of the study for the participant

chosen: a unique code (assigned randomly) identifying the specific random sequences of coin flips for each of the 8 coins (which were generated randomly according to the true coin biases before the start of the experiment).

subCondition: In the Symmetric condition, keeps track of the weighting between public and private signals subCondition=1 for 9 shared, 3 private signals, w=0.25; subCondition=2 for 9 shared, 9 private signals, w=0.5; subCondition=3 for 3 shared, 9 private signals, w=0.75. In the Nested-Symmetric condition, keeps track of the proportion of mavens subCondition=1 for p=0.25; subCondition=2 for p=0.5; subCondition=3 for p=0.75. In the Nested condition, keeps track of the proportion of experts subCondition=1 for p=0.25; subCondition=2 for p=0.5; subCondition=3 for p=0.75.

weight: the relative information weight w on private versus shared signals for those 8 coins

proportion: the probability that a participant in the crowd was assigned to the expert or maven role

numShared: the number of total flips observed in the shared signal for those 8 coins

numPrivate: the number of total flips observed in the private signal for those 8 coins

C1SharedSum through C8SharedSum: the number of heads observed in the shared signal for for coins 1 through 8

C1PrivateSum through C8PrivateSum:  the number of heads observed in the private signal for coins 1 through 8 (only relevant for experts and mavens)

theta1 through theta8: true bias of coins 1 through 8

pos1 through pos8: order in which coins 1 through 8 were displayed to that participant

type: role assigned to the participant for all 8 coins. “N” means layperson and type="S" means "expert" or "maven" in N and NS, respectively.

Quiz1: Participants were asked “Suppose that a biased coin has an 80% chance of coming up heads. How many heads would you expect to see in 100 new flips of the coin?” The possible answers are coded as 1 = “5”, 2 = “10”, 3 = “20”, 4 = “50”, 5 = “80”. (The correct answer was 5).

Quiz2: Participants were asked “Suppose that a coin has a 40% chance of coming up heads. If you flip the coin 100 times, which statement is true?” The possible answers are coded as 1 = “The chances of getting 91 heads is exactly the same as the chances of getting 43 heads”, 2 = “Any number of heads is technically possible, but the chances are good that I will get between 30 and 50 heads”, 3 = “It is impossible to get fewer than 10 heads”, 4 = “There is a 50% chance that the coin will come up tails”, 5 = “I will definitely see exactly 40 heads”. (The correct answer was 2).

age: participant’s stated age

female: indicator variable for whether participant stated their gender as female
	
device: Participants were asked “What type of device are you using to take this survey?” The possible answers are coded as 1 = “Desktop computer”, 2 = “Laptop”, 3 = “Tablet”, 4 = “Smartphone”, 5 = “Other (please indicate)”

f1 through f8: participant’s own forecast for coins 1 through 8

g1 through g8: participant’s guess of others’ average forecast for coins 1 through 8
 




