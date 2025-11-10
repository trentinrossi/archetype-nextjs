import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/credit-cards/:cardNumber - Get credit card by number
export async function GET(
  request: NextRequest,
  { params }: { params: { cardNumber: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/credit-cards/${params.cardNumber}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching credit card:', error);
    return NextResponse.json({ error: 'Failed to fetch credit card' }, { status: 500 });
  }
}

// PUT /api/credit-cards/:cardNumber - Update credit card
export async function PUT(
  request: NextRequest,
  { params }: { params: { cardNumber: string } }
) {
  try {
    const body = await request.json();
    
    // Validate card status if provided
    if (body.cardStatus && !/^[A-Z]$/.test(body.cardStatus)) {
      return NextResponse.json(
        { error: 'Card status must be a single uppercase letter' },
        { status: 400 }
      );
    }
    
    // Validate expiry month if provided
    if (body.expiryMonth && !/^(0[1-9]|1[0-2])$/.test(body.expiryMonth)) {
      return NextResponse.json(
        { error: 'Expiry month must be between 01 and 12' },
        { status: 400 }
      );
    }
    
    // Validate expiry year if provided
    if (body.expiryYear && !/^\d{4}$/.test(body.expiryYear)) {
      return NextResponse.json(
        { error: 'Expiry year must be 4 digits' },
        { status: 400 }
      );
    }
    
    const response = await forwardAuthRequest(
      `/api/credit-cards/${params.cardNumber}`,
      'PUT',
      request,
      body
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error updating credit card:', error);
    return NextResponse.json({ error: 'Failed to update credit card' }, { status: 500 });
  }
}

// DELETE /api/credit-cards/:cardNumber - Delete credit card
export async function DELETE(
  request: NextRequest,
  { params }: { params: { cardNumber: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/credit-cards/${params.cardNumber}`,
      'DELETE',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(
      { message: 'Credit card deleted successfully' },
      { status: result.status }
    );
  } catch (error) {
    console.error('Error deleting credit card:', error);
    return NextResponse.json({ error: 'Failed to delete credit card' }, { status: 500 });
  }
}
