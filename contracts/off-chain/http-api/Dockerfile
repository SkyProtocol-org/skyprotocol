# Dockerfile
# Use the official Node.js image
FROM node:18-alpine

# Set the working directory
WORKDIR /usr/src/app

# Copy application code
COPY . .

# Expose the port that the server listens on
EXPOSE 3030

# Install (erm, probably better way to do this...)
CMD ["npm", "i"]

# Command to start the server
CMD ["node", "server.mjs"]
