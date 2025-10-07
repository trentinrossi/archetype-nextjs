// src/app/api/auth/signon/route.ts
import { NextRequest, NextResponse } from 'next/server';
import { SignonRequestDTO, SignonResponseDTO, ValidationError } from '@/types/userSecurity';

// Mock user database - In production, this would be replaced with actual database calls
const MOCK_USERS = [
  {
    id: 'ADMIN001',
    firstName: 'System',
    lastName: 'Administrator',
    password: 'ADMIN123',
    userType: 'A',
    isActive: true,
    createdAt: '2024-01-01T00:00:00Z',
    updatedAt: '2024-01-01T00:00:00Z',
    lastLoginAt: null
  },
  {
    id: 'USER001',
    firstName: 'John',
    lastName: 'Doe',
    password: 'USER123',
    userType: 'U',
    isActive: true,
    createdAt: '2024-01-01T00:00:00Z',
    updatedAt: '2024-01-01T00:00:00Z',
    lastLoginAt: null
  },
  {
    id: 'INACTIVE',
    firstName: 'Inactive',
    lastName: 'User',
    password: 'PASS123',
    userType: 'U',
    isActive: false,
    createdAt: '2024-01-01T00:00:00Z',
    updatedAt: '2024-01-01T00:00:00Z',
    lastLoginAt: null
  }
];

// JWT token generation (simplified - in production use proper JWT library)
function generateToken(userId: string): { token: string; refreshToken: string; expiresAt: string } {
  const expiresAt = new Date(Date.now() + 24 * 60 * 60 * 1000).toISOString(); // 24 hours
  const token = `jwt_token_${userId}_${Date.now()}`;
  const refreshToken = `refresh_token_${userId}_${Date.now()}`;
  
  return { token, refreshToken, expiresAt };
}

// Validate signon input following COSGN00C business rules
function validateSignonInput(request: SignonRequestDTO): ValidationError[] {
  const errors: ValidationError[] = [];

  // Validate User ID (SEC-USR-ID: 8 chars)
  if (!request.userId || request.userId.trim().length === 0) {
    errors.push({ field: 'userId', message: 'Please enter User ID' });
  } else if (request.userId.trim().length > 8) {
    errors.push({ field: 'userId', message: 'User ID must not exceed 8 characters' });
  } else if (!/^[A-Za-z0-9]+$/.test(request.userId.trim())) {
    errors.push({ field: 'userId', message: 'User ID must contain only letters and numbers' });
  }

  // Validate Password (SEC-USR-PWD: 8 chars)
  if (!request.password || request.password.length === 0) {
    errors.push({ field: 'password', message: 'Please enter Password' });
  } else if (request.password.length > 20) {
    errors.push({ field: 'password', message: 'Password must not exceed 20 characters' });
  }

  return errors;
}

// Find user by ID (case-insensitive)
function findUserById(userId: string) {
  return MOCK_USERS.find(user => 
    user.id.toUpperCase() === userId.toUpperCase()
  );
}

// COSGN00C Business Logic Implementation
export async function POST(request: NextRequest) {
  try {
    const body: SignonRequestDTO = await request.json();

    // Validate input
    const validationErrors = validateSignonInput(body);
    if (validationErrors.length > 0) {
      return NextResponse.json(
        {
          success: false,
          message: 'Invalid input provided',
          errors: validationErrors
        } as SignonResponseDTO,
        { status: 400 }
      );
    }

    const { userId, password } = body;
    const normalizedUserId = userId.toUpperCase().trim();

    // Find user in USRSEC equivalent
    const user = findUserById(normalizedUserId);

    if (!user) {
      return NextResponse.json(
        {
          success: false,
          message: 'User not found',
          errors: [{ field: 'userId', message: 'User not found' }]
        } as SignonResponseDTO,
        { status: 401 }
      );
    }

    // Check if user account is active
    if (!user.isActive) {
      return NextResponse.json(
        {
          success: false,
          message: 'User account is inactive',
          errors: [{ field: 'userId', message: 'User account is inactive' }]
        } as SignonResponseDTO,
        { status: 403 }
      );
    }

    // Validate password
    if (user.password !== password) {
      return NextResponse.json(
        {
          success: false,
          message: 'Wrong Password',
          errors: [{ field: 'password', message: 'Wrong Password' }]
        } as SignonResponseDTO,
        { status: 401 }
      );
    }

    // Authentication successful - generate tokens
    const { token, refreshToken, expiresAt } = generateToken(user.id);

    // Update last login timestamp (in production, this would update the database)
    user.lastLoginAt = new Date().toISOString();

    // Prepare user data for response (exclude password)
    const userDTO = {
      id: user.id,
      firstName: user.firstName,
      lastName: user.lastName,
      userType: user.userType,
      isActive: user.isActive,
      createdAt: user.createdAt,
      updatedAt: user.updatedAt,
      lastLoginAt: user.lastLoginAt,
      fullName: `${user.firstName} ${user.lastName}`.trim()
    };

    // Successful authentication response
    const response: SignonResponseDTO = {
      success: true,
      user: userDTO,
      token,
      refreshToken,
      expiresAt,
      message: `Welcome, ${user.firstName} ${user.lastName}!`
    };

    return NextResponse.json(response, { status: 200 });

  } catch (error) {
    console.error('Signon API error:', error);
    
    return NextResponse.json(
      {
        success: false,
        message: 'Internal server error',
        errors: [{ field: 'general', message: 'System error occurred during authentication' }]
      } as SignonResponseDTO,
      { status: 500 }
    );
  }
}

// Handle OPTIONS request for CORS
export async function OPTIONS(request: NextRequest) {
  return new NextResponse(null, {
    status: 200,
    headers: {
      'Access-Control-Allow-Origin': '*',
      'Access-Control-Allow-Methods': 'POST, OPTIONS',
      'Access-Control-Allow-Headers': 'Content-Type, Authorization',
    },
  });
}