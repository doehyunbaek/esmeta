const monster1 = {
    eyeCount: 4,
};
const handler1 = {
    getOwnPropertyDescriptor(target, prop) {
        return undefined
    },
};

const proxy1 = new Proxy(monster1, handler1);
proxy1.hasOwnProperty("noseCount"); // false
