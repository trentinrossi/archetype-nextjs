import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// POST /api/credit-cards/validate-backward-navigation - Validate navigation to previous page
export async function POST(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const page = searchParams.get('page');
    
    if (!page) {
      return NextResponse.json(
        { error: 'page is required' },
        { status: 400 }
      );
    }
    
    const response = await forwardAuthRequest(
      `/api/v1/credit-cards/validate-backward-navigation?page=${page}`,
      'POST',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error validating backward navigation:', error);
    return NextResponse.json(
      { error: 'Failed to validate backward navigation' },
      { status: 500 }
    );
  }
}
