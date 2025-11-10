import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

/**
 * GET /api/cards/:cardNumber - Get card by card number
 */
export async function GET(
  request: NextRequest,
  { params }: { params: { cardNumber: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/cards/${params.cardNumber}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching card:', error);
    return NextResponse.json(
      { error: 'Failed to fetch card' },
      { status: 500 }
    );
  }
}

/**
 * PUT /api/cards/:cardNumber - Update card
 */
export async function PUT(
  request: NextRequest,
  { params }: { params: { cardNumber: string } }
) {
  try {
    const body = await request.json();
    const response = await forwardAuthRequest(
      `/cards/${params.cardNumber}`,
      'PUT',
      request,
      body
    );
    const result = await handleAuthApiResponse(response);
    
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error updating card:', error);
    return NextResponse.json(
      { error: 'Failed to update card' },
      { status: 500 }
    );
  }
}

/**
 * DELETE /api/cards/:cardNumber - Delete card
 */
export async function DELETE(
  request: NextRequest,
  { params }: { params: { cardNumber: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/cards/${params.cardNumber}`,
      'DELETE',
      request
    );
    const result = await handleAuthApiResponse(response);
    
    return NextResponse.json(
      { message: 'Card deleted successfully' },
      { status: result.status }
    );
  } catch (error) {
    console.error('Error deleting card:', error);
    return NextResponse.json(
      { error: 'Failed to delete card' },
      { status: 500 }
    );
  }
}
