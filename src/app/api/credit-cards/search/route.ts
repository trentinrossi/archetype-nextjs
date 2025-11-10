import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/credit-cards/search - Search credit cards by card number pattern
export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const cardNumberPattern = searchParams.get('cardNumberPattern');
    const page = searchParams.get('page') || '0';
    const size = searchParams.get('size') || '20';
    const sort = searchParams.get('sort') || '';
    
    if (!cardNumberPattern) {
      return NextResponse.json(
        { error: 'Card number pattern is required' },
        { status: 400 }
      );
    }
    
    let queryString = `/api/credit-cards/search?cardNumberPattern=${cardNumberPattern}&page=${page}&size=${size}`;
    if (sort) {
      queryString += `&sort=${sort}`;
    }
    
    const response = await forwardAuthRequest(queryString, 'GET', request);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error searching credit cards:', error);
    return NextResponse.json({ error: 'Failed to search credit cards' }, { status: 500 });
  }
}
