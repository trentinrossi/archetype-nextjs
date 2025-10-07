// src/api/auth/exit/route.ts
import { NextRequest, NextResponse } from 'next/server';

export async function POST(request: NextRequest) {
  try {
    // Handle PF3 key press for graceful exit (COSGN00C business rule)
    return NextResponse.json({
      success: true,
      message: 'Thank you for using CardDemo application'
    }, { status: 200 });

  } catch (error) {
    console.error('Exit error:', error);
    return NextResponse.json({
      success: false,
      errorCode: 'SYSTEM_ERROR',
      errorMessage: 'Internal server error'
    }, { status: 500 });
  }
}