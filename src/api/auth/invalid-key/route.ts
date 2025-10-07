// src/api/auth/invalid-key/route.ts
import { NextRequest, NextResponse } from 'next/server';

export async function POST(request: NextRequest) {
  try {
    // Handle invalid key press during authentication (COSGN00C business rule)
    return NextResponse.json({
      success: false,
      errorCode: 'INVALID_KEY',
      errorMessage: 'Invalid key pressed'
    }, { status: 400 });

  } catch (error) {
    console.error('Invalid key error:', error);
    return NextResponse.json({
      success: false,
      errorCode: 'SYSTEM_ERROR',
      errorMessage: 'Internal server error'
    }, { status: 500 });
  }
}