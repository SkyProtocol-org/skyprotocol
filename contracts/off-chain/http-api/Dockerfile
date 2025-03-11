# Dockerfile
# Use the official Node.js image
FROM node:18-alpine

# Set the working directory
WORKDIR /usr/src/app

# Copy application code
COPY . .

# Expose the port that the server listens on
EXPOSE 3030

# Run
ENTRYPOINT ["/usr/src/app/start.sh"]
