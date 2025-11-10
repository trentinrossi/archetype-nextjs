import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

/**
 * GET /api/cards - Get paginated list of cards with optional filtering
 * Supports filtering by accountId and cardNumber
 * Implements pagination with default page size of 7 (as per COBOL screen)
 */
export async function GET(request: NextRequest) {
  try {
    const searchParams = request.nextUrl.searchParams;
    const accountId = searchParams.get('accountId');
    const cardNumber = searchParams.get('cardNumber');
    const page = searchParams.get('page') || '0';
    const size = searchParams.get('size') || '7';
    
    // Build query string
    const queryParams = new URLSearchParams();
    if (accountId) queryParams.append('accountId', accountId);
    if (cardNumber) queryParams.append('cardNumber', cardNumber);
    queryParams.append('page', page);
    queryParams.append('size', size);
    
    const response = await forwardAuthRequest(
      `/cards/list?${queryParams.toString()}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching cards:', error);
    return NextResponse.json(
      { error: 'Failed to fetch cards' },
      { status: 500 }
    );
  }
}

/**
 * POST /api/cards - Create a new card
 */
export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const response = await forwardAuthRequest('/cards', 'POST', request, body);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error creating card:', error);
    return NextResponse.json(
      { error: 'Failed to create card' },
      { status: 500 }
    );
  }
}
