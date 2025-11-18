import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

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

export async function PUT(
  request: NextRequest,
  { params }: { params: { cardNumber: string } }
) {
  try {
    const body = await request.json();
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
    return NextResponse.json({ message: 'Credit card deleted successfully' }, { status: result.status });
  } catch (error) {
    console.error('Error deleting credit card:', error);
    return NextResponse.json({ error: 'Failed to delete credit card' }, { status: 500 });
  }
}
