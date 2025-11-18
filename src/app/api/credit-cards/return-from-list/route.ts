import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const queryString = searchParams.toString();
    const endpoint = queryString 
      ? `/api/credit-cards/return-from-list?${queryString}` 
      : '/api/credit-cards/return-from-list';
    
    const response = await forwardAuthRequest(endpoint, 'GET', request);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error returning from list:', error);
    return NextResponse.json({ error: 'Failed to return from list' }, { status: 500 });
  }
}
