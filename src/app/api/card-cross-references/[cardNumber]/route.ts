import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// DELETE /api/card-cross-references/:cardNumber - Delete card cross reference
export async function DELETE(
  request: NextRequest,
  { params }: { params: { cardNumber: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/card-cross-references/${params.cardNumber}`,
      'DELETE',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json({ message: 'Card cross reference deleted successfully' }, { status: result.status });
  } catch (error) {
    console.error('Error deleting card cross reference:', error);
    return NextResponse.json({ error: 'Failed to delete card cross reference' }, { status: 500 });
  }
}
