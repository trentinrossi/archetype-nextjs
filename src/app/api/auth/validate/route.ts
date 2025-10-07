import { NextRequest, NextResponse } from 'next/server';
import { SignonRequestDTO, ValidationResponseDTO } from '@/types/user';

// Mock user database
const mockUsers = [
  {
    userId: 'ADMIN001',
    password: 'ADMIN123',
    userType: 'ADMIN' as const,
    active: true
  },
  {
    userId: 'USER001',
    password: 'USER123',
    userType: 'GENERAL' as const,
    active: true
  }
];

export async function POST(request: NextRequest) {
  try {
    const body: SignonRequestDTO = await request.json();
    
    // Validate input
    if (!body.userId || body.userId.trim() === '') {
      const response: ValidationResponseDTO = {
        valid: false,
        message: 'Please enter User ID'
      };
      return NextResponse.json(response, { status: 400 });
    }
    
    if (!body.password || body.password.trim() === '') {
      const response: ValidationResponseDTO = {
        valid: false,
        message: 'Please enter Password'
      };
      return NextResponse.json(response, { status: 400 });
    }
    
    // Convert to uppercase for case-insensitive lookup
    const userIdUpper = body.userId.toUpperCase();
    
    // Find user
    const user = mockUsers.find(u => u.userId === userIdUpper);
    
    if (!user) {
      const response: ValidationResponseDTO = {
        valid: false,
        message: 'User not found'
      };
      return NextResponse.json(response, { status: 401 });
    }
    
    // Check if user is active
    if (!user.active) {
      const response: ValidationResponseDTO = {
        valid: false,
        message: 'User account is inactive'
      };
      return NextResponse.json(response, { status: 401 });
    }
    
    // Validate password
    if (user.password !== body.password) {
      const response: ValidationResponseDTO = {
        valid: false,
        message: 'Wrong Password'
      };
      return NextResponse.json(response, { status: 401 });
    }
    
    const response: ValidationResponseDTO = {
      valid: true,
      message: 'Validation successful'
    };
    
    return NextResponse.json(response, { status: 200 });
    
  } catch (error) {
    console.error('Validation error:', error);
    const response: ValidationResponseDTO = {
      valid: false,
      message: 'Internal server error'
    };
    return NextResponse.json(response, { status: 500 });
  }
}