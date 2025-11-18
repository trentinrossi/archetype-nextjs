import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const searchTerm = searchParams.get('searchTerm');
    
    if (!searchTerm) {
      return NextResponse.json(
        { error: 'Search term is required' },
        { status: 400 }
      );
    }
    
    const queryString = searchParams.toString();
    const response = await forwardAuthRequest(
      `/api/accounts/search?${queryString}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error searching accounts:', error);
    return NextResponse.json({ error: 'Failed to search accounts' }, { status: 500 });
  }
}
