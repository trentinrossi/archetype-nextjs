import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/card-cross-references/:accountId/:cardNumber - Get specific card cross-reference
export async function GET(
  request: NextRequest,
  { params }: { params: { accountId: string; cardNumber: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/card-cross-references/${params.accountId}/${params.cardNumber}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching card cross-reference:', error);
    return NextResponse.json({ error: 'Failed to fetch card cross-reference' }, { status: 500 });
  }
}

// DELETE /api/card-cross-references/:accountId/:cardNumber - Delete card cross-reference
export async function DELETE(
  request: NextRequest,
  { params }: { params: { accountId: string; cardNumber: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/card-cross-references/${params.accountId}/${params.cardNumber}`,
      'DELETE',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json({ message: 'Card cross-reference deleted successfully' }, { status: result.status });
  } catch (error) {
    console.error('Error deleting card cross-reference:', error);
    return NextResponse.json({ error: 'Failed to delete card cross-reference' }, { status: 500 });
  }
}
