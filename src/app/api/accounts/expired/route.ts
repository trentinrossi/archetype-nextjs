import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/accounts/expired - Get all expired accounts
export async function GET(request: NextRequest) {
  try {
    const response = await forwardAuthRequest(
      '/api/accounts/expired',
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching expired accounts:', error);
    return NextResponse.json(
      { error: 'Failed to fetch expired accounts' },
      { status: 500 }
    );
  }
}
