import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/credit-cards/account/:accountId/search - Search credit cards by account and card number pattern
export async function GET(
  request: NextRequest,
  { params }: { params: { accountId: string } }
) {
  try {
    // Validate account ID (11 digits)
    if (!/^\d{11}$/.test(params.accountId)) {
      return NextResponse.json(
        { error: 'Account ID must be exactly 11 digits' },
        { status: 400 }
      );
    }
    
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
    
    let queryString = `/api/v1/credit-cards/account/${params.accountId}/search?cardNumberPattern=${cardNumberPattern}&page=${page}&size=${size}`;
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
