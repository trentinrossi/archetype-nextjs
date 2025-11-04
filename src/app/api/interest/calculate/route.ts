import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// POST /api/interest/calculate - Calculate monthly interest
export async function POST(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const processingDate = searchParams.get('processingDate');
    
    if (!processingDate) {
      return NextResponse.json({ error: 'Processing date is required' }, { status: 400 });
    }

    const response = await forwardAuthRequest(
      `/api/interest/calculate?processingDate=${encodeURIComponent(processingDate)}`,
      'POST',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error calculating interest:', error);
    return NextResponse.json({ error: 'Failed to calculate interest' }, { status: 500 });
  }
}
