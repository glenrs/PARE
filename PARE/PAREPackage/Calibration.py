import numpy as np
np.random.seed(0)

import matplotlib
matplotlib.use("svg")
import matplotlib.pyplot as plt
from matplotlib import cm
#matplotlib inline

from sklearn import datasets
from sklearn.naive_bayes import GaussianNB
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier
from sklearn.svm import LinearSVC
from sklearn.calibration import calibration_curve, CalibratedClassifierCV
from sklearn.metrics import (brier_score_loss, precision_score, recall_score,
                                     f1_score, log_loss)
from sklearn.cross_validation import train_test_split

# <!-- collapse=True -->
X, y = datasets.make_classification(n_samples=100000, n_features=20,
                                            n_informative=2, n_redundant=2)

train_samples = 100  # Samples used for training the models

X_train = X[:train_samples]
X_test = X[train_samples:]
y_train = y[:train_samples]
y_test = y[train_samples:]

# Create classifiers
lr = LogisticRegression()
gnb = GaussianNB()
svc = LinearSVC(C=1.0)
rfc = RandomForestClassifier(n_estimators=100)

plt.figure(figsize=(9, 9))
ax1 = plt.subplot2grid((3, 1), (0, 0), rowspan=2)
ax2 = plt.subplot2grid((3, 1), (2, 0))

ax1.plot([0, 1], [0, 1], "k:", label="Perfectly calibrated")
for clf, name in [(lr, 'Logistic'),
                          (gnb, 'Naive Bayes'),
                                            (svc, 'Support Vector Classification'),
                                                              (rfc, 'Random Forest')]:
        clf.fit(X_train, y_train)
            if hasattr(clf, "predict_proba"):
                        prob_pos = clf.predict_proba(X_test)[:, 1]
                            else:  # use decision function
                                        prob_pos = clf.decision_function(X_test)
                                                prob_pos = \
                                                                    (prob_pos - prob_pos.min()) / (prob_pos.max() - prob_pos.min())
                                                                        fraction_of_positives, mean_predicted_value = \
                                                                                        calibration_curve(y_test, prob_pos, n_bins=10)

                                                                                            ax1.plot(mean_predicted_value, fraction_of_positives, "s-",
                                                                                                                 label="%s" % (name, ))

                                                                                                ax2.hist(prob_pos, range=(0, 1), bins=10, label=name,
                                                                                                                     histtype="step", lw=2)

                                                                                                ax1.set_ylabel("Fraction of positives")
