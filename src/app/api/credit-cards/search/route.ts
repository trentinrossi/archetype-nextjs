import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const queryString = searchParams.toString();
    
    if (!queryString) {
      return NextResponse.json(
        { error: 'At least one search criterion must be provided' },
        { status: 400 }
      );
    }
    
    const response = await forwardAuthRequest(
      `/api/credit-cards/search?${queryString}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error searching credit cards:', error);
    return NextResponse.json({ error: 'Failed to search credit cards' }, { status: 500 });
  }
}
