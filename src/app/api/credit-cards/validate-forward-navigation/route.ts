import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// POST /api/credit-cards/validate-forward-navigation - Validate navigation to next page
export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    
    const response = await forwardAuthRequest(
      '/api/v1/credit-cards/validate-forward-navigation',
      'POST',
      request,
      body
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error validating forward navigation:', error);
    return NextResponse.json(
      { error: 'Failed to validate forward navigation' },
      { status: 500 }
    );
  }
}
