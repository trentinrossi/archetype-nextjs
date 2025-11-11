import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// POST /api/credit-cards/validate-selection - Validate single selection
export async function POST(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const selectionCount = searchParams.get('selectionCount');
    
    if (!selectionCount) {
      return NextResponse.json(
        { error: 'selectionCount is required' },
        { status: 400 }
      );
    }
    
    const response = await forwardAuthRequest(
      `/api/v1/credit-cards/validate-selection?selectionCount=${selectionCount}`,
      'POST',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error validating selection:', error);
    return NextResponse.json(
      { error: 'Failed to validate selection' },
      { status: 500 }
    );
  }
}
