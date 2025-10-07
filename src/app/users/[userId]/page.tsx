'use client';

import React from 'react';
import { UpdateUserForm } from '@/components/user/UpdateUserForm';
import { useRouter, useParams } from 'next/navigation';

export default function UpdateUserPage() {
  const router = useRouter();
  const params = useParams();
  const userId = params.userId as string;

  // Handle user updated successfully
  const handleUserUpdated = (updatedUserId: string) => {
    console.log(`User ${updatedUserId} updated successfully`);
    // Stay on update form (COBOL business logic)
  };

  // Handle update user exit
  const handleUpdateUserExit = () => {
    router.push('/users');
  };

  return (
    <div className="w-full">
      <UpdateUserForm
        selectedUserId={userId}
        onUserUpdated={handleUserUpdated}
        onExit={handleUpdateUserExit}
      />
    </div>
  );
}