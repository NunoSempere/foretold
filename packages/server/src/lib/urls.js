const clientUrl = process.env.NODE_ENV === "development" ? "http://localhost:1234" : "https://www.foretold.io"

module.exports = { clientUrl };
