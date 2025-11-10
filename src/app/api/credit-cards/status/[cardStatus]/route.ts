import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/credit-cards/status/:cardStatus - Get credit cards by status
export async function GET(
  request: NextRequest,
  { params }: { params: { cardStatus: string } }
) {
  try {
    // Validate card status (single uppercase letter)
    if (!/^[A-Z]$/.test(params.cardStatus)) {
      return NextResponse.json(
        { error: 'Card status must be a single uppercase letter' },
        { status: 400 }
      );
    }
    
    const { searchParams } = new URL(request.url);
    const page = searchParams.get('page') || '0';
    const size = searchParams.get('size') || '20';
    const sort = searchParams.get('sort') || '';
    
    let queryString = `/api/credit-cards/status/${params.cardStatus}?page=${page}&size=${size}`;
    if (sort) {
      queryString += `&sort=${sort}`;
    }
    
    const response = await forwardAuthRequest(queryString, 'GET', request);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching credit cards by status:', error);
    return NextResponse.json({ error: 'Failed to fetch credit cards' }, { status: 500 });
  }
}
