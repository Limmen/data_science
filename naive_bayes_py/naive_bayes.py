import numpy as np

def train():
    return learn_model()

def learn_model():
    features, classes, feature_values, data = training_data()
    feature_likelihoods, class_likelihoods = compute_likelihoods(features, classes, data, feature_values)
    conditionals = calculate_conditionals(features, feature_likelihoods, class_likelihoods, data)
    return feature_likelihoods, class_likelihoods, conditionals, features, classes, feature_values

def classify(x, model):
    feature_likelihoods, class_likelihoods, conditionals, features, classes, feature_values = model
    class_posteriors = []
    for i, c in enumerate(classes):
        prior = class_likelihoods[i]
        x_conditionals = [conditionals[f_i][x_i][i] for f_i, x_i in enumerate(x)]
        naive_likelihood = np.prod(np.array(x_conditionals))
        c_posterior = naive_likelihood*prior
        class_posteriors.append(c_posterior)
    class_idx = np.argmax(np.array(class_posteriors))
    return class_idx, classes[class_idx]

def calculate_conditionals(features, feature_likelihoods, class_likelihoods, data):
    num_examples = len(data)
    conditionals_1 = {}
    for i, feature in enumerate(features):
        conditionals_2 = {}
        for feature_val in range(len(feature_likelihoods[i])):
            probs = []
            for class_j in range(len(class_likelihoods)):
                joint_freq = len(list(filter(lambda x: x[i] == feature_val and x[-1] == class_j, list(data))))
                joint_prob = joint_freq/num_examples
                class_likelihood = class_likelihoods[class_j]
                conditional = joint_prob/class_likelihood
                probs.append(conditional)
            conditionals_2[feature_val] = probs
        conditionals_1[i] = conditionals_2
    return conditionals_1

def compute_likelihoods(features, classes, data, feature_values):
    num_examples = len(data)
    feature_likelihoods = {}
    for i, feature in enumerate(features):
        feature_data = data[:, i]
        unique, counts = np.unique(feature_data, return_counts=True)
        count_d = dict(zip(unique, counts))
        f_likelihoods = []
        for j, val in enumerate(feature_values[features[i]]):
            count = count_d[j]
            f_likelihoods.append(count/num_examples)
        feature_likelihoods[i] = f_likelihoods

    classes_data = data[:, -1]
    unique, counts = np.unique(classes_data, return_counts=True)
    count_d = dict(zip(unique, counts))
    class_likelihoods = []
    for j, val in enumerate(classes):
        count = count_d[j]
        class_likelihoods.append(count / num_examples)

    return feature_likelihoods, class_likelihoods

def training_data():
    features = ["outlook", "temperature", "humidity", "windy"]
    classes = ["play", "not_play"]
    feature_values = {
        "outlook": ["sunny", "overcast", "rain"],
        "temperature": ["mild", "hot", "cool"],
        "humidity": ["high", "normal"],
        "windy": [False, True]
    }
    data = np.array([
        [0, 1, 0, 0, 0],
        [0, 1, 0, 1, 1],
        [1, 1, 0, 0, 1],
        [2, 0, 0, 0, 0],
        [2, 2, 1, 0, 0],
        [2, 2, 1, 1, 1],
        [1, 2, 1, 1, 0],
        [0, 0, 0, 0, 1],
        [0, 2, 1, 0, 0],
        [2, 0, 1, 0, 0],
        [0, 0, 1, 1, 0],
        [1, 0, 0, 1, 0],
        [1, 1, 1, 0, 0],
        [2, 0, 0, 1, 1],
    ])
    return features, classes, feature_values, data


if __name__ == "__main__":
    model = train()
    feature_likelihoods, class_likelihoods, conditionals, features, classes, feature_values = model
    class_id, class_label = classify([1, 1, 0, 1], model)
    print((class_id, class_label))
    class_id, class_label = classify([2, 2, 1, 1], model)
    print((class_id, class_label))