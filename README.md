# Outros Modelos de Machine Learning


# script 001
<img width="512" alt="cat_age" src="https://user-images.githubusercontent.com/32683908/234896848-0a964845-d641-4c59-9122-a8cad4e3e3da.png">
<img width="512" alt="cat_fare" src="https://user-images.githubusercontent.com/32683908/234896856-d5528744-1b09-4414-856b-6db4f624a890.png">
<img width="512" alt="plotando_arvore" src="https://user-images.githubusercontent.com/32683908/234896858-742b0481-e9eb-42b1-beac-5e25f289b0fc.png">

# script 002

#### CurvaROC treino

<img width="512" alt="roc_treino" src="https://user-images.githubusercontent.com/32683908/234896944-ca5de4b8-fceb-42af-9dc2-41c120bb1f5b.png">

#### CurvaROC teste

<img width="512" alt="roc_teste" src="https://user-images.githubusercontent.com/32683908/234896939-01449e81-3dc3-4b2f-b988-440ca5cf9770.png">


## pós-poda (Grid Search)

<img width="512" alt="arvore" src="https://user-images.githubusercontent.com/32683908/234896924-14a9fd48-ee02-4643-a618-3bc9dade5e64.png">



#### Grid Search

```
            CP nsplit rel error xerror       xstd
1  0.432000000      0     1.000  1.000 0.05005229
2  0.032000000      1     0.568  0.568 0.04230546
3  0.030000000      2     0.536  0.556 0.04197515
4  0.020000000      4     0.476  0.524 0.04105572
5  0.018000000      5     0.456  0.528 0.04117381
6  0.012000000      7     0.420  0.500 0.04032751
7  0.008000000     10     0.384  0.496 0.04020277
8  0.004000000     16     0.332  0.484 0.03982263
9  0.002666667     41     0.224  0.536 0.04140723
10 0.002000000     44     0.216  0.540 0.04152259
11 0.001600000     83     0.136  0.588 0.04283915
12 0.001333333    114     0.084  0.640 0.04413324
13 0.000800000    128     0.064  0.664 0.04468739
14 0.000000000    143     0.048  0.668 0.04477721

```

#### cp_min

```
         CP      nsplit   rel error      xerror        xstd 
 0.00400000 16.00000000  0.33200000  0.48400000  0.03982263

```

#### Modelo com cp_min

```
arvore_poda <- rpart::rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                            data=treino,
                            method='class',
                            xval=0,
                            control = rpart.control(cp = cp_min, 
                                                    minsplit = 1, 
                                                    maxdepth = 30)
```

#### CurvaROC treino

<img width="512" alt="CurvaROC_pos_poda_treino" src="https://user-images.githubusercontent.com/32683908/234896936-6af0e0e9-a721-4dae-b34e-50f1e2874206.png">

#### CurvaROC teste

<img width="512" alt="CurvaROC_pos_poda_teste" src="https://user-images.githubusercontent.com/32683908/234896933-8c5dad8a-d2da-4fc5-b579-ab7d25b7402d.png">
