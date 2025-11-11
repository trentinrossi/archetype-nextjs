import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/card-cross-references/card/:cardNumber - Get all card cross-references for a card
export async function GET(
  request: NextRequest,
  { params }: { params: { cardNumber: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/card-cross-references/card/${params.cardNumber}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching card cross-references by card:', error);
    return NextResponse.json({ error: 'Failed to fetch card cross-references' }, { status: 500 });
  }
}

// DELETE /api/card-cross-references/card/:cardNumber - Delete all card cross-references for a card
export async function DELETE(
  request: NextRequest,
  { params }: { params: { cardNumber: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/card-cross-references/card/${params.cardNumber}`,
      'DELETE',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json({ message: 'Card cross-references deleted successfully' }, { status: result.status });
  } catch (error) {
    console.error('Error deleting card cross-references:', error);
    return NextResponse.json({ error: 'Failed to delete card cross-references' }, { status: 500 });
  }
}
