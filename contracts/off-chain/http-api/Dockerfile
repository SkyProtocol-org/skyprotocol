# Dockerfile
# Use the official Node.js image
FROM node:18-alpine

# Set the working directory
WORKDIR /usr/src/app

# Copy application code
COPY server.js .

# Expose the port that the server listens on
EXPOSE 3030

# Command to start the server
CMD ["node", "server.js"]
