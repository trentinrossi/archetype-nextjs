'use client';

import React from 'react';
import { DeleteUserForm } from '@/components/user/DeleteUserForm';
import { useRouter, useParams } from 'next/navigation';

export default function DeleteUserPage() {
  const router = useRouter();
  const params = useParams();
  const userId = params.userId as string;

  // Handle user deleted successfully
  const handleUserDeleted = (deletedUserId: string) => {
    console.log(`User ${deletedUserId} deleted successfully`);
    // Redirect to user list after successful deletion
    router.push('/users');
  };

  // Handle delete user exit
  const handleDeleteUserExit = () => {
    router.push('/users');
  };

  return (
    <div className="w-full">
      <DeleteUserForm
        selectedUserId={userId}
        onUserDeleted={handleUserDeleted}
        onExit={handleDeleteUserExit}
      />
    </div>
  );
}