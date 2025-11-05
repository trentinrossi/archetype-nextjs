# Deployment Guide - Card Transaction Lifecycle Management

This guide provides step-by-step instructions for deploying the Card Transaction Lifecycle Management application.

## Prerequisites

- Node.js 18.x or higher
- npm or yarn package manager
- Backend API running and accessible
- Git (for version control)

---

## Local Development Setup

### 1. Clone the Repository
```bash
git clone <repository-url>
cd card-transaction-management
```

### 2. Install Dependencies
```bash
npm install
# or
yarn install
```

### 3. Configure Environment Variables
Create a `.env.local` file in the root directory:

```bash
cp .env.example .env.local
```

Edit `.env.local` and update the API URL:
```env
NEXT_PUBLIC_API_BASE_URL=http://localhost:8080/api
```

### 4. Run Development Server
```bash
npm run dev
# or
yarn dev
```

The application will be available at `http://localhost:3000`

---

## Production Build

### 1. Build the Application
```bash
npm run build
# or
yarn build
```

This creates an optimized production build in the `.next` folder.

### 2. Test Production Build Locally
```bash
npm run start
# or
yarn start
```

---

## Deployment Options

### Option 1: Vercel (Recommended for Next.js)

#### Prerequisites
- Vercel account (free tier available)
- Vercel CLI installed: `npm i -g vercel`

#### Steps
1. **Login to Vercel**
   ```bash
   vercel login
   ```

2. **Deploy**
   ```bash
   vercel
   ```

3. **Configure Environment Variables**
   - Go to Vercel Dashboard → Your Project → Settings → Environment Variables
   - Add: `NEXT_PUBLIC_API_BASE_URL` with your production API URL

4. **Redeploy with Environment Variables**
   ```bash
   vercel --prod
   ```

#### Custom Domain (Optional)
- Go to Vercel Dashboard → Your Project → Settings → Domains
- Add your custom domain and follow DNS configuration instructions

---

### Option 2: Docker Deployment

#### Create Dockerfile
```dockerfile
FROM node:18-alpine AS base

# Install dependencies only when needed
FROM base AS deps
RUN apk add --no-cache libc6-compat
WORKDIR /app

COPY package.json package-lock.json ./
RUN npm ci

# Rebuild the source code only when needed
FROM base AS builder
WORKDIR /app
COPY --from=deps /app/node_modules ./node_modules
COPY . .

ENV NEXT_TELEMETRY_DISABLED 1

RUN npm run build

# Production image, copy all the files and run next
FROM base AS runner
WORKDIR /app

ENV NODE_ENV production
ENV NEXT_TELEMETRY_DISABLED 1

RUN addgroup --system --gid 1001 nodejs
RUN adduser --system --uid 1001 nextjs

COPY --from=builder /app/public ./public
COPY --from=builder --chown=nextjs:nodejs /app/.next/standalone ./
COPY --from=builder --chown=nextjs:nodejs /app/.next/static ./.next/static

USER nextjs

EXPOSE 3000

ENV PORT 3000

CMD ["node", "server.js"]
```

#### Build and Run Docker Container
```bash
# Build image
docker build -t card-transaction-app .

# Run container
docker run -p 3000:3000 \
  -e NEXT_PUBLIC_API_BASE_URL=http://your-api-url/api \
  card-transaction-app
```

---

### Option 3: Traditional Server Deployment

#### Prerequisites
- Node.js installed on server
- PM2 or similar process manager
- Nginx or Apache for reverse proxy

#### Steps

1. **Build the Application**
   ```bash
   npm run build
   ```

2. **Transfer Files to Server**
   ```bash
   rsync -avz --exclude 'node_modules' ./ user@server:/path/to/app/
   ```

3. **Install Dependencies on Server**
   ```bash
   ssh user@server
   cd /path/to/app
   npm ci --production
   ```

4. **Configure Environment Variables**
   ```bash
   echo "NEXT_PUBLIC_API_BASE_URL=http://your-api-url/api" > .env.production
   ```

5. **Start with PM2**
   ```bash
   pm2 start npm --name "card-transaction-app" -- start
   pm2 save
   pm2 startup
   ```

6. **Configure Nginx Reverse Proxy**
   ```nginx
   server {
       listen 80;
       server_name yourdomain.com;

       location / {
           proxy_pass http://localhost:3000;
           proxy_http_version 1.1;
           proxy_set_header Upgrade $http_upgrade;
           proxy_set_header Connection 'upgrade';
           proxy_set_header Host $host;
           proxy_cache_bypass $http_upgrade;
       }
   }
   ```

---

### Option 4: AWS Deployment

#### Using AWS Amplify

1. **Connect Repository**
   - Go to AWS Amplify Console
   - Click "New App" → "Host web app"
   - Connect your Git repository

2. **Configure Build Settings**
   ```yaml
   version: 1
   frontend:
     phases:
       preBuild:
         commands:
           - npm ci
       build:
         commands:
           - npm run build
     artifacts:
       baseDirectory: .next
       files:
         - '**/*'
     cache:
       paths:
         - node_modules/**/*
   ```

3. **Add Environment Variables**
   - Go to App Settings → Environment Variables
   - Add: `NEXT_PUBLIC_API_BASE_URL`

4. **Deploy**
   - Amplify will automatically deploy on every push to main branch

---

## Environment Variables

### Required Variables
```env
NEXT_PUBLIC_API_BASE_URL=<your-api-url>
```

### Optional Variables
```env
NEXT_PUBLIC_APP_NAME=Card Transaction Lifecycle Management
NEXT_PUBLIC_APP_VERSION=1.0.0
NODE_ENV=production
```

---

## Post-Deployment Checklist

### 1. Verify API Connection
- [ ] Test transaction list page loads
- [ ] Test creating a new transaction
- [ ] Test viewing transaction details
- [ ] Test account management
- [ ] Test card management
- [ ] Test cross-reference management

### 2. Test Core Functionality
- [ ] All CRUD operations work
- [ ] Pagination functions correctly
- [ ] Filtering works as expected
- [ ] Error handling displays properly
- [ ] Loading states appear correctly
- [ ] Form validation works

### 3. Performance Checks
- [ ] Page load times are acceptable
- [ ] API response times are reasonable
- [ ] Images and assets load quickly
- [ ] No console errors in browser

### 4. Security Checks
- [ ] API endpoints require authentication
- [ ] Sensitive data is not exposed in URLs
- [ ] HTTPS is enabled (production)
- [ ] CORS is properly configured

### 5. Browser Compatibility
- [ ] Chrome/Edge (latest)
- [ ] Firefox (latest)
- [ ] Safari (latest)
- [ ] Mobile browsers

---

## Monitoring and Maintenance

### Logging
- Check application logs regularly
- Monitor API error rates
- Track user activity

### Updates
```bash
# Update dependencies
npm update

# Check for security vulnerabilities
npm audit

# Fix vulnerabilities
npm audit fix
```

### Backup
- Regular database backups (backend)
- Version control for code
- Document configuration changes

---

## Troubleshooting

### Common Issues

#### 1. API Connection Failed
**Problem**: Cannot connect to backend API

**Solutions**:
- Verify `NEXT_PUBLIC_API_BASE_URL` is correct
- Check backend API is running
- Verify CORS settings on backend
- Check network/firewall rules

#### 2. Build Fails
**Problem**: `npm run build` fails

**Solutions**:
- Clear `.next` folder: `rm -rf .next`
- Clear node_modules: `rm -rf node_modules && npm install`
- Check Node.js version: `node --version`
- Review build error messages

#### 3. Environment Variables Not Working
**Problem**: Environment variables not being read

**Solutions**:
- Ensure variables start with `NEXT_PUBLIC_`
- Restart development server after changes
- Rebuild application for production
- Check `.env.local` file exists and is not in `.gitignore`

#### 4. 404 Errors on Refresh
**Problem**: Page not found when refreshing

**Solutions**:
- Configure server to handle client-side routing
- For Nginx, add try_files directive
- For Apache, enable mod_rewrite

---

## Support

For issues or questions:
1. Check the README.md file
2. Review the IMPLEMENTATION_SUMMARY.md
3. Check application logs
4. Contact the development team

---

## Rollback Procedure

If deployment issues occur:

1. **Vercel**: Use the Deployments tab to rollback to previous version
2. **Docker**: Keep previous image tags and redeploy
3. **Traditional Server**: Keep previous build and switch PM2 process
4. **AWS Amplify**: Use the Amplify console to redeploy previous version

---

**Last Updated**: 2024
**Version**: 1.0.0
