'use client';

import React from 'react';
import { AddUserForm } from '@/components/user/AddUserForm';
import { useRouter } from 'next/navigation';

export default function AddUserPage() {
  const router = useRouter();

  // Handle user added successfully
  const handleUserAdded = (userId: string) => {
    console.log(`User ${userId} added successfully`);
    // Stay on add user form for additional entries (COBOL business logic)
  };

  // Handle add user exit
  const handleAddUserExit = () => {
    router.push('/');
  };

  return (
    <div className="w-full">
      <AddUserForm
        onUserAdded={handleUserAdded}
        onExit={handleAddUserExit}
      />
    </div>
  );
}